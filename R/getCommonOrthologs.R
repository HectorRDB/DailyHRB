# Load and download the gencode files if needed
#' .getGencode Function
#'
#' Download and pre-process the gencode files
#'
#' @param ref the gencode reference for species.
#' Either a number (the gencode version) or the location of the file.
#' @param species The species
#' @importFrom curl curl_download
#' @importFrom dplyr filter select mutate
#' @importFrom tidyr separate
#' @importFrom stringr str_replace_all
#' @importFrom magrittr %>%
.getGencode <- function(ref, species) {
  if (!file.exists(ref)) {
    url <- paste0(
      "ftp://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_", species,
      "/release_", ref, "/gencode.v", ref, ".annotation.gtf.gz"
      )
    curl::curl_download(url, destfile = "/tmp/gencode.gft.gz")
    loc <- "/tmp/gencode.gft.gz"
  } else {
    loc <- ref
  }
  gencode <- read.table(loc, skip = 5, sep = c("\t"))
  colnames(gencode) <- c("chr", "annot", "type1", "start", "end", ".", "strand",
                       "ID", "infos")
  gencode <- gencode %>%
    dplyr::filter(type1 == "gene") %>%
    dplyr::select(infos) %>%
    tidyr::separate(infos, c("gene_id", "gene_type", "gene_name",
                             "level","havana_gene", "tags"),
                    ";", extra = "merge", fill = "right") %>%
    dplyr::select("gene_id", "gene_name") %>%
    dplyr::mutate(Ensembl = stringr::str_replace_all(gene_id, "gene_id ", ""),
                  Symbol = stringr::str_replace_all(gene_name, " gene_name ",
                                                    "")) %>%
    dplyr::select(Ensembl, Symbol)

  return(gencode)
}

# Get the list of orthologs
#' .Orthologs Function
#'
#' For a list of genes, find the orthologs in the other species
#'
#' @param genes The list of genes for which we want the orthologs
#' @param gencode the gencode file
#' @param species The species
#' @import org.Hs.eg.db org.Mm.eg.db
#' @importFrom dplyr inner_join full_join
#' @importFrom biomaRt useMart getLDS
#' @importFrom magrittr %>%
.Orthologs <- function(genes, gencode, species){
  # Get the genes we actually use
  df <- data.frame(Symbol = genes,
                   ID = 1:length(genes),
                   stringsAsFactors = F) %>%
    dplyr::inner_join(gencode, by = "Symbol")
  colnames(df) <- paste(species, colnames(df), sep = "_")
  x <- df[, paste(species, "Ensembl", sep = "_")]

  # Find the orthologs per say
  spec <- ifelse(species == "human", "hsapiens", "mmusculus")
  other_species <- ifelse(species == "human", "mouse", "human")

  marts <- list(human = biomaRt::useMart("ensembl",
                                         dataset = "hsapiens_gene_ensembl"),
                mouse = biomaRt::useMart("ensembl",
                                         dataset = "mmusculus_gene_ensembl"))

  genes <- biomaRt::getLDS(attributes = c("ensembl_gene_id_version"),
                           filters = "ensembl_gene_id_version",
                           values = x ,
                           mart = marts[[species]],
                           attributesL = c("ensembl_gene_id_version"),
                           martL = marts[[other_species]],
                           uniqueRows = F)
  colnames(genes) <- paste(c(species, other_species), "Ensembl", sep = "_")
  df <- full_join(df, genes)
  return(df)
}

# Get the list of genes with an orthologs
#' common Function
#'
#' For the list of genes restrict to the common orthologs
#'
#' @param gHuman The list of human genes
#' @param gMouse The list of mouse genes
#' @import dplyr tidyr
#' @importFrom magrittr %>%
.common <- function(gHuman, gMouse) {
  gFull <- dplyr::inner_join(gHuman, gMouse) %>%
    dplyr::group_by(human_Symbol, mouse_Symbol) %>%
    dplyr::slice(1) %>%
    dplyr::select(-human_Ensembl, -mouse_Ensembl) %>%
    dplyr::group_by(human_Symbol) %>%
    dplyr::arrange(mouse_Symbol) %>%
    dplyr::mutate(mouse_Group_Name = paste0(mouse_Symbol, collapse = "_")) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(mouse_Symbol) %>%
    dplyr::arrange(human_Symbol) %>%
    dplyr::mutate(human_Group_Name = paste0(human_Symbol, collapse = "_"))

  indsMouse <- gFull %>%
    dplyr::ungroup() %>%
    dplyr::select(mouse_ID, mouse_Group_Name) %>%
    dplyr::distinct()

  indsHuman <- gFull %>%
    dplyr::ungroup() %>%
    dplyr::select(human_ID, human_Group_Name) %>%
    dplyr::distinct()

  ids <- gFull %>% ungroup() %>%
    dplyr::select(mouse_Group_Name, human_Group_Name) %>%
    distinct()

  return(list("mouse" = indsMouse, "human" = indsHuman,
              "common" = ids))
}

# Restrict the matrix to common orthologs
#' getCommonOrthologs Function
#'
#' This function takes as input two count matrices from human and mouse and
#' return the matrices subset to the ortholog genes
#'
#' @param countMouse the count matrix from the mouse
#' @param countHuman the count matrix from the human
#' @param refMouse the gencode reference for mouse.
#' Either a number (the gencode version) or the location of the file.
#' @param refHuman the gencode reference for human.
#' Either a number (the gencode version) or the location of the file
#' @param mergeFunction How to merge multimaps. See details.
#' Default to \code{mean}
#' @import dplyr tidyr
#' @importFrom magrittr %>%
#' @details
#' When several genes from one species map to at least one gene from the other,
#' counts are merged by taking the mean expression in each cell accross those
#' genes. A different function can be specified using the `mergeFunction`
#' argument. This function is inspired adapted from
## https://www.r-bloggers.com/converting-mouse-to-human-gene-names-with-biomart-package/
#' @export
getCommonOrthologs <- function(countMouse, countHuman, refMouse, refHuman,
                               mergeFunction = mean) {
  # Get the genes
  message("Getting the data from gencode")
  message(".. human")
  gencodeHuman <- .getGencode(refHuman, "human")
  message(".. mouse")
  gencodeMouse <- .getGencode(refMouse, "mouse")
  gHuman <- .Orthologs(rownames(countHuman), gencodeHuman, "human")
  gMouse <- .Orthologs(rownames(countMouse), gencodeMouse, "mouse")

  # Subset to common genes
  message("Tranforming the count matrices")
  inds <- .common(gHuman, gMouse)

  countsMouse <- cbind(countMouse[inds$mouse$mouse_ID, ], inds$mouse) %>%
    as.data.frame() %>%
    dplyr::select(-mouse_ID) %>%
    tidyr::gather(key = "cell", value = "count", -mouse_Group_Name) %>%
    dplyr::group_by(cell, mouse_Group_Name) %>%
    dplyr::summarise(count = mergeFunction(count)) %>%
    tidyr::spread(key = cell, value = count)
  rownames(countsMouse) <- countsMouse$mouse_Group_Name
  countsMouse <- countsMouse %>% dplyr::select(-mouse_Group_Name)

  countsHuman <- cbind(countHuman[inds$human$human_ID, ], inds$human) %>%
    as.data.frame() %>%
    dplyr::select(-human_ID) %>%
    tidyr::gather(key = "cell", value = "count", -human_Group_Name) %>%
    dplyr::group_by(cell, human_Group_Name) %>%
    dplyr::summarise(count = mergeFunction(count)) %>%
    tidyr::spread(key = cell, value = count)
  rownames(countsHuman) <- countsHuman$human_Group_Name
  countsHuman <- countsHuman %>% dplyr::select(-human_Group_Name)

  return(list("mouse" = countsMouse, "human" = countsHuman,
              "ids" = inds$common))
}

