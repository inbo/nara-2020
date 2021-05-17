renv::restore()
library(rmarkdown)
library(bookdown)
library(here)
library(assertthat)
library(git2r)
library(git2rdata)
library(bibtex)
check_metadata <- function(path, theme) {
  meta <- yaml_front_matter(path)
  assert_that(
    has_name(meta, "title"),
    msg = paste("'title' ontbreekt in", path)
  )
  assert_that(
    is.string(meta$title),
    msg = paste(
      "'title' moet een tekstvector zijn met slechs 1 element in", path
    )
  )

  assert_that(
    has_name(meta, "date"),
    msg = paste("'date' ontbreekt in", path)
  )
  assert_that(
    is.time(as.POSIXct(meta$date, format = "%FT%T")),
    msg = paste("'date' moet in het YYYY-MM-DDTHH:MM:SS formaat zijn in", path)
  )

  assert_that(
    has_name(meta, "lang"),
    msg = paste("'lang' ontbreekt in", path)
  )
  assert_that(
    is.string(meta$lang),
    msg = paste(
      "'lang' moet een tekstvector zijn met slechs 1 element in ", path
    )
  )
  assert_that(
    meta$lang %in% c("nl", "en"),
    msg = "'lang' moet 'nl' of 'en' zijn"
  )

  assert_that(
    has_name(meta, "thema"),
    msg = paste("'thema' ontbreekt in", path)
  )
  assert_that(
    is.character(meta$thema),
    msg = paste("'thema' moet een tekstvector zijn in", path)
  )
  assert_that(
    length(meta$thema) > 0,
    msg = paste("Minstens een thema is verplicht in", path)
  )
  assert_that(
    anyDuplicated(meta$thema) == 0,
    msg = paste("Thema's moeten uniek zijn in", path)
  )
  if (meta$lang == "nl") {
    assert_that(
      all(meta$thema %in% theme$Term),
      msg = paste(
        "Verkeerd thema in", path, "\nToegestane thema's zijn",
        paste0("'", sort(theme$Term), "'", collapse = ", ")
      )
    )
    pure_id <- theme$PureId[theme$Term %in% meta$thema]
  } else {
    assert_that(
      all(meta$thema %in% theme$TermEN),
      msg = paste(
        "Verkeed thema in", path, "\nToegestane thema's zijn",
        paste0("'", sort(theme$TermEN), "'", collapse = ", ")
      )
    )
    pure_id <- theme$PureId[theme$TermEN %in% meta$thema]
  }

  assert_that(
    has_name(meta, "keywords"),
    msg = paste("'keywords' ontbreekt in", path)
  )
  assert_that(
    is.character(meta$keywords),
    msg = paste("'keywords' moet een tekstvector zijn in", path)
  )
  assert_that(
    length(meta$keywords) > 0,
    msg = paste("Voeg minstens een keyword toe in", path)
  )

  assert_that(
    has_name(meta, "tab"),
    msg = paste("'tab' ontbreekt in", path)
  )
  assert_that(
    is.string(meta$tab),
    msg = paste("'tab' moet een tekstvector zijn met slechs 1 element in", path)
  )
  assert_that(
    meta$tab %in% c("indicator", "metadata", "generic"),
    msg = paste("'tab' moet 'indicator', 'metadata' of 'generic' zijn in", path)
  )

  assert_that(
    has_name(meta, "verantwoordelijke"),
    msg = paste("'verantwoordelijke' ontbreekt in", path)
  )
  assert_that(
    is.character(meta$verantwoordelijke),
    msg = paste("'verantwoordelijke' moet een tekstvector zijn in", path)
  )
  assert_that(
    length(meta$verantwoordelijke) > 0,
    msg = paste("voeg een verantwoordelijke toe in", path)
  )
  assert_that(
    all(grepl("^.+ <.+@.+>$", meta$verantwoordelijke)),
    msg =
    "verantwoordelijk moet het formaat `Voornaam Naam <email@domeinnaam>` zijn"
  )
  return(pure_id)
}

check_structuur <- function(path) {
  rmd <- readLines(path)
  start_chunk <- grep("^```\\{r.*}", rmd)
  end_chunk <- grep("^```[:space:]?$", rmd)
  assert_that(
    length(start_chunk) == length(end_chunk),
    all(start_chunk < end_chunk),
    msg = paste(path, "heeft een probleem met R chunks")
  )
  for (i in rev(seq_along(start_chunk))) {
    rmd <- c(
      head(rmd, start_chunk[i] - 1),
      "{r code chunk}",
      tail(rmd, length(rmd) - end_chunk[i])
    )
  }
  koppen <- rmd[grepl("^[[:space:]]?#", rmd)]
  assert_that(
    !any(grepl("^[[:space:]]+#", koppen)),
    msg = paste(path, "Koppen moeten met een '#' beginnen", sep = "\n")
  )
  assert_that(
    !any(grepl("^# .*", koppen)),
    msg = paste(path, "Kop 1 is niet toegestaan", sep = "\n")
  )
  assert_that(
    !any(grepl("^####+ .*", koppen)),
    msg = paste(path, "Enkel kop 2 en kop 3 zijn toegestaan", sep = "\n")
  )
  assert_that(
    !any(grepl("[[:space:]]+$", koppen)),
    msg = paste(
      path,
      "Witruimte op het einde van een kop is niet toegestaan",
      paste(koppen[grepl("[[:space:]]+$", koppen)], collapse = "\n"),
      sep = "\n"
    )
  )
  if (grepl("source/generiek", path)) {
    return(TRUE)
  }
  meta <- yaml_front_matter(path)
  template <- switch(
    meta$tab,
    indicator = readLines(here("template", "indicator.Rmd")),
    metadata = readLines(here("template", "metadata.Rmd"))
  )
  template_koppen <- template[grepl("^[:space:]?#", template)]
  h2 <- koppen[grepl("^## ", koppen)]
  verplicht <- template_koppen[!grepl(" \\{.*\\}", template_koppen)]
  optioneel <- template_koppen[grepl(" \\{.*\\}", template_koppen)]
  optioneel <- gsub(" \\{.*\\}", "", optioneel)
  assert_that(
    all(verplicht %in% h2),
    msg = paste(
      path,
      "Verplichte kop ontbreekt",
      paste(verplicht[!verplicht %in% h2], collapse = "\n"),
      sep = "\n"
    )
  )

  assert_that(
    all(h2 %in% c(optioneel, verplicht)),
    msg = paste(
      path,
      "Onderstaande koppen van niveau 2 zijn niet toegestaan",
      paste(h2[!h2 %in% c(optioneel, verplicht)], collapse = "\n"),
      sep = "\n"
    )
  )
  volgorde <- gsub(" \\{.*\\}", "", template_koppen)
  volgorde <- volgorde[volgorde %in% h2]
  assert_that(
    all(h2 == volgorde),
    msg = paste(
      path,
      "De koppen van niveau 2 staan niet in onderstaande volgorde",
      paste(volgorde, collapse = "\n"),
      sep = "\n"
    )
  )
  ref_titel <- template_koppen[grepl("indien referenties", template_koppen)]
  ref_titel <- gsub(" \\{.*\\}", "", ref_titel)

  ref_aanwezig <- ref_titel %in% h2

  if (!has_name(meta, "bibliography")) {
    assert_that(
      !ref_aanwezig,
      msg = paste(
        path, "Referentietitel niet toegestaan zonder referenties", sep = "\n"
      )
    )
    return(TRUE)
  }

  refs <- unlist(strsplit(rmd, " "))
  refs_aanwezig <- unique(refs[grepl("\\[?@.*", refs)])
  refs_aanwezig <- refs_aanwezig[!grepl("<.*?@.*?>", refs_aanwezig)]
  if (length(refs_aanwezig) > 0) {
    bib <- normalizePath(file.path(dirname(path), meta$bibliography))
    bib <- read.bib(bib, encoding = "UTF-8")
    refs_beschikbaar <- paste0("@", names(bib))
    beschikbaar <- sapply(refs_beschikbaar, grepl, refs_aanwezig)
    beschikbaar <- rowSums(beschikbaar) > 0
    assert_that(
      all(beschikbaar),
      msg = paste(
        "Ontbrekende referentie in bibliografie van", path, sep = "\n",
        paste(refs_aanwezig[!beschikbaar], collapse = "\n")
      )
    )
  }
  assert_that(
    !any(xor(ref_aanwezig, length(refs_aanwezig) > 0)),
    msg = paste(
      path,
    "Referentietitel en BibTex referenties moeten beiden aan- of afwezig zijn",
      sep = "\n"
    )
  )
  if (!ref_aanwezig) {
    return(TRUE)
  }
  assert_that(
    tail(rmd, 1) == ref_titel,
    msg = paste(
      path,
      "Bibliografie gevonden zonder referentietitel op laatste lijn.
Verwijder `bibliography` uit de yaml header of voeg de referentietitel toe.",
sep = "\n"
    )
  )
}

changed_path <- function(path = ".") {
  repo <- repository(path)
  local <- tree(last_commit(repo))
  upstream <- tree(lookup_commit(branches(repo)[["origin/main"]]))
  changes <- diff(local, upstream)
  changes <- lapply(
    changes$files,
    function(x) {
      c(x$old_file, x$new_file)
    }
  )
  changes <- unlist(changes)
  unique(dirname(changes[grepl("^source.*\\.Rmd$", changes)]))
}

render_one <- function(path, pure_id, root_dir) {
  setwd(dirname(path))
  message(path)

  # render HTML
  meta <- yaml_front_matter(path)
  meta$verantwoordelijke <- gsub("^.*<(.*)>$", "\\1", meta$verantwoordelijke)
  meerweten <- lapply(
    meta$verantwoordelijke,
    pandoc_variable_arg,
    name = "meerweten"
  )
  output_format <- html_document(
    self_contained = FALSE,
    keep_md = TRUE,
    template = here("template/default.html"),
    pandoc_args = c(
      pandoc_variable_arg(
        "pure_id", paste(pure_id[[path]], collapse = ", ")
      ),
      "--email-obfuscation=javascript",
      unlist(meerweten)
    ),
    params = list(render_indicator = TRUE),
    lib_dir = "libs"
  )
  output_format$knitr$opts_chunk$fig.path <- file.path(
    "..", basename(dirname(path)), gsub("\\.Rmd$", "_files/", basename(path))
  )
  z <- render(path, output_format = output_format)

  # move libs path
  html <- readLines(z)
  html <- gsub("(<(script src|link href)=\")libs/", "\\1../libs/", html)
  writeLines(html, z)

  target <- gsub(
    file.path(root_dir, "source"),
    file.path(root_dir, "publish"),
    dirname(path)
  )
  dir.create(target, showWarnings = FALSE, recursive = TRUE)
  dir.create(
    file.path(root_dir, "publish/libs"),
    showWarnings = FALSE
  )

  # copy html
  file.copy(z, file.path(target, basename(z)), overwrite = TRUE)
  file.remove(z)

  # copy libs
  file.copy(
    from = file.path(dirname(z), "libs"),
    to = normalizePath(file.path(target, "..")),
    recursive = TRUE,
    overwrite = FALSE
  )

  # read md
  z <- gsub(".html", ".md", z)
  md <- readLines(z)
  file.remove(z)

  #copy internal links
  base_links <- md[grep("\\[.*\\]\\(.*\\)", md)]
  links <- character(0)
  while (length(base_links) > 0) {
    links <- c(links, gsub(".*?\\[.*?\\]\\((.*?)\\).*", "\\1", base_links))
    base_links <- gsub(".*?\\[.*?\\]\\((.*?)\\)(.*)", "\\2", base_links)
    base_links <- base_links[grep("\\[.*\\]\\(.*\\)", base_links)]
  }
  links <- links[!grepl("^https?://", links)]
  generiek <- gsub("#", "", unique(links[grepl("^#", links)]))
  if (length(generiek)) {
    generiek_ok <- generiek %in% meta$add_generic
    if (any(!generiek_ok)) {
      stop(
        "link naar generieke metadata zonder vermelding in add_generic: ",
        generiek[!generiek_ok],
        "\nin ", path
      )
    }
  }
  links <- links[!grepl("^#", links)]
  ok <- grepl("^\\.\\./.*", links)
  if (any(!ok)) {
    stop(
      "alle interne links moeten starten met '../mapnaam/'\nfoute links: ",
      paste(links[!ok], collapse = ", "),
      "\nin ", path
    )
  }

  link_ok <- file_test(
    "-f",
    normalizePath(file.path(dirname(path), links), mustWork = FALSE)
  )
  if (!all(link_ok)) {
    stop("Links naar bestanden die niet gevonden worden.
Problematische links: ",
paste0("\"", links[!link_ok], "\"", collapse = ", "),
"\nin ", path)
  }
  links <- normalizePath(
    file.path(dirname(path), links),
    mustWork = TRUE,
    winslash = "/"
  )
  if (length(links) > 0) {
    link_target <- gsub(
      file.path(root_dir, "source"),
      file.path(root_dir, "publish"),
      links
    )
    vapply(
      unique(dirname(link_target)),
      dir.create,
      logical(1),
      recursive = TRUE,
      showWarnings = FALSE
    )
    file.copy(links, link_target, overwrite = TRUE)
  }

  return(invisible(NULL))
}

check_publicatiedatum <- function(path) {
  dirs <- unique(dirname(path))
  dirs <- dirs[basename(dirs) != "generiek"]
  aantal_data <- sapply(
    dirs,
    function(i) {
      x <- list.files(i, pattern = "\\.[Rr]md$", full.names = TRUE)
      x <- lapply(x, yaml_front_matter)
      unique(sapply(x, `[[`, "date"))
    }
  )
  aantal_data <- aantal_data[sapply(aantal_data, length) > 1]
  aantal_data <- sapply(aantal_data, paste, collapse = " - ")
  aantal_data <- paste(basename(names(aantal_data)), aantal_data)
  assert_that(
    length(aantal_data) == 0,
    msg = paste(
      c(
  "Indicatoren met verschillende publicatiedatum tussen indicator en metadata.",
        aantal_data),
      collapse = "\n"
    )
  )
  return(TRUE)
}

check_hoofdstuk <- function(path) {
  correct <- read_vc("template/hoofdstuk")
  dirs <- unique(dirname(path))
  dirs <- dirs[basename(dirs) != "generiek"]
  sapply(
    dirs,
    function(i) {
      input <- list.files(i, pattern = "\\.[Rr]md$", full.names = TRUE)
      x <- lapply(input, yaml_front_matter)
      x <- setNames(x, input)
      x <- sapply(x, `[[`, "hoofdstuk")
      ok <- sapply(x, is.number)
      assert_that(
        all(ok),
        msg = paste0(
          "'hoofdstuk' moet een geheel getal zijn in\n",
          paste(names(x)[!ok], collapse = "\n")
        )
      )
      ok <- x %in% correct$hoofdstuk
      assert_that(
        all(ok),
        msg = paste0(
          "verkeerd 'hoofdstuk' nummer in\n",
          paste(names(x)[!ok], collapse = "\n")
        )
      )
      assert_that(
        length(unique(x)) == 1,
        msg = paste0(
          "verschillende nummers in 'hoofdstuk' in\n",
          paste(names(x), collapse = "\n")
        )
      )
    }
  )
  return(invisible(NULL))
}

on_main <- function(path = ".") {
  if (Sys.getenv("GITHUB_ACTIONS") == "true") {
    return(Sys.getenv("GITHUB_REF") == "refs/heads/main")
  }
  if (!interactive()) {
    return(FALSE)
  }
  branch_name <- repository_head(path)$name
  !is.null(branch_name) && branch_name == "main"
}

render_all <- function(everything = FALSE) {
  if (everything || on_main(here())) {
    path <- "source"
  } else {
    path <- changed_path(here())
  }
  to_do <- list.files(here(path), pattern = ".rmd", ignore.case = TRUE,
                      recursive = TRUE, full.names = TRUE)
  to_do <- normalizePath(to_do)
  theme <- read_vc("template/thema")
  pure_id <- lapply(to_do, check_metadata, theme = theme)
  names(pure_id) <- to_do
  sapply(to_do, check_structuur)
  check_publicatiedatum(to_do)
  check_hoofdstuk(to_do)
  root_dir <- getwd()
  on.exit(setwd(root_dir), add = TRUE)
  rendered <- sapply(to_do, render_one, pure_id = pure_id, root_dir = root_dir)
  return(invisible(rendered))
}
