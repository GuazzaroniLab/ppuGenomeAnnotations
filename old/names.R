names <- rs_gb |>
  dplyr::select(-dplyr::starts_with("start"),
                -dplyr::starts_with("end"),
                -dplyr::starts_with("feature"),
                -locus_tag) |>

  dplyr::filter(! stringr::str_detect(name_product.refseq, pattern = "^$")) |>
  dplyr::mutate(
    # make names all lowercase (for consistency)
    name_product.refseq = tolower(name_product.refseq),
    name_product.genbank = tolower(name_product.genbank),

    # remove weird characters / , - + ( and )
    name_product.refseq = stringr::str_replace_all(name_product.refseq, "\\/|,|\\-|\\(|\\)|\\+|\\[|\\]", " "),
    name_product.genbank = stringr::str_replace_all(name_product.genbank, "\\/|,|\\-|\\(|\\)|\\+|\\[|\\]", " "),

    # turns refseq into a regex pattern
    regex.refseq = stringr::str_squish(name_product.refseq),
    regex.refseq = stringr::str_remove_all(regex.refseq, "\\d{1}(?=\\s)"),
    regex.refseq = stringr::str_replace_all(regex.refseq, "(?<=\\s).(?=\\s)|(?<=\\s).(?=$)", ""),
    regex.refseq = stringr::str_squish(regex.refseq),
    regex.refseq = stringr::str_replace_all(regex.refseq, "\\s", replacement = "|"),


    # matches the regex to the genbank accesion
    matches.n = stringr::str_count(name_product.genbank, regex.refseq),
    matches = stringr::str_extract_all(name_product.genbank, regex.refseq, simplify = TRUE)
  )

