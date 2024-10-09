## code to prepare `decretos` dataset goes here


#usethis::use_data(decretos, overwrite = TRUE)

ler_arq <- function(x) {
  x |>
    xml2::read_html() |>
    xml2::xml_find_first("//body") |>
    xml2::xml_text()
}

x <- fs::dir_ls("data-raw/html")[1]

decretos <- fs::dir_ls("data-raw/html") |>
  purrr::map(ler_arq) |>
  tibble::enframe() |>
  dplyr::mutate(value = stringr::str_trim(as.character(value)))

da_decretos <- decretos |>
  dplyr::mutate(value = stringr::str_split(value, "(?=Art\\.[[:space:]]+[0-9]+)")) |>
  tidyr::unnest(value) |>
  dplyr::group_by(name) |>
  dplyr::mutate(doc = seq_len(dplyr::n())) |>
  dplyr::ungroup() |>
  dplyr::transmute(
    decreto = name,
    id_artigo = doc,
    artigo = tolower(value)
  )

da_decretos$artigo[50] |> cat()
da_decretos |>
  dplyr::filter(decreto == unique(decreto)[3]) |>
  print(n = 100)

da_decretos |>
  dplyr::count(decreto)

embeds <- da_decretos |>
  dplyr::group_split(decreto) |>
  purrr::set_names(unique(da_decretos$decreto)) |>
  purrr::map(\(x) {
    aux_embed <- openai::create_embedding(
      model = "text-embedding-3-large",
      input = x$artigo
    )
    da_emb <- aux_embed$data |>
      tibble::tibble() |>
      tidyr::unnest(embedding) |>
      dplyr::select(-object) |>
      dplyr::transmute(
        name = x$id_artigo[index + 1],
        value = embedding
      ) |>
      dplyr::group_by(name) |>
      dplyr::mutate(id_embedding = seq_len(dplyr::n())) |>
      dplyr::ungroup() |>
      tidyr::pivot_wider() |>
      dplyr::select(-id_embedding)
    da_emb
  })

embeds[[1]]

da_embeds <- embeds |>
  purrr::map(\(x) {
    tidyr::pivot_longer(x, dplyr::everything(), names_to = "artigo", values_to = "embed")
  }) |>
  purrr::list_rbind(names_to = "file") |>
  dplyr::mutate(
    embed_id = seq_len(dplyr::n()),
    .by = c(file, artigo),
    .before = embed
  ) |>
  dplyr::arrange(file, artigo)


# distancias <- embeds[[1]] |>
#   as.matrix() |>
#   lsa::cosine()

# distancias <- 1 - distancias

# da_distancias <- distancias |>
#   tibble::as_tibble(rownames = "code1") |>
#   tidyr::pivot_longer(-code1, names_to = "code2", values_to = "dist")

# # dendrogram

# hc <- hclust(as.dist(distancias), method = "ward.D2")
# dend <- as.dendrogram(hc)

# cluster::silhouette(cutree(hc, k = 9), distancias) |>
#   plot()

# da_result <- cutree(hc, 15) |>
#   tibble::enframe() |>
#   dplyr::group_by(value) |>
#   dplyr::summarise(
#     n = dplyr::n(),
#     codes = paste(name, collapse = ", ")
#   )

calcular_clusters <- function(id_decreto) {
  usethis::ui_info("Calculando clusters para decreto {id_decreto}")
  da_model <- embeds[[id_decreto]] |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "artigo", values_to = "embed") |>
    dplyr::mutate(
      embed_id = seq_len(dplyr::n()),
      .by = c(artigo),
      .before = embed
    ) |>
    tidyr::pivot_wider(names_from = embed_id, values_from = embed) |>
    dplyr::select(-artigo)

  res_kmeans <- NbClust::NbClust(
    da_model, method = "kmeans",
    min.nc = 4, max.nc = 10,
    index = "silhouette"
  )

  res_ward <- NbClust::NbClust(
    da_model, method = "ward.D2",
    min.nc = 4, max.nc = 10,
    index = "silhouette"
  )

  # res_ward_cosine <- NbClust::NbClust(
  #   #da_model,
  #   diss = distancias, distance = NULL,
  #   method = "ward.D2",
  #   min.nc = 4, max.nc = 10,
  #   index = "silhouette"
  # )

  # fazer análise quali depois
  da_decretos_cluster <- da_decretos |>
    dplyr::filter(decreto == unique(decreto)[id_decreto]) |>
    dplyr::mutate(
      cluster_kmeans = res_kmeans$Best.partition,
      cluster_ward = res_ward$Best.partition
    )
  da_decretos_cluster
}

library(patchwork)

results_clusters <- purrr::map(1:3, calcular_clusters)

results_clusters_lab <- results_clusters |>
  purrr::list_rbind() |>
  dplyr::mutate(decreto_lab = dplyr::case_when(
    decreto == "data-raw/html/D12002.html" ~ "Decreto 12.002/2024",
    decreto == "data-raw/html/D9191compilado.html" ~ "Decreto 9.191/2017",
    decreto == "data-raw/html/D4176impressao.html" ~ "Decreto 4.176/2002",
  )) |>
  dplyr::group_split(decreto)


set.seed(2)

grafico_tsne <- function(i) {
  p1 <- embeds[[i]] |>
    M3C::tsne(labels = factor(results_clusters[[i]]$cluster_kmeans)) +
    ggplot2::scale_colour_viridis_d() +
    ggplot2::labs(
      title = glue::glue("{results_clusters_lab[[i]]$decreto_lab[1]} - Kmeans"),
      x = "", y = ""
    )
  p2 <- embeds[[i]] |>
    M3C::tsne(labels = factor(results_clusters[[i]]$cluster_ward)) +
    ggplot2::scale_colour_viridis_d() +
    ggplot2::labs(
      title = glue::glue("{results_clusters_lab[[i]]$decreto_lab[1]} - Hierárquico"),
      x = "", y = ""
    )
  p1 / p2
}

purrr::map(c(2, 3, 1), grafico_tsne) |>
  patchwork::wrap_plots(ncol = 3)


# tsne para os clusters
library(M3C)
?M3C::tsne


# pedir para o gemini criar uma descrição do que foi feito
# passar cada cluster de decretos para o gemini analisar


pegar_infos_gemini <- function(txt, prompt) {
  result <- httr::POST(
    "https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-pro-latest:generateContent?key=AIzaSyCgd-PDZ1zAwR5nK8lZqnru_kJ9L8m3hRc",
    body = list(
      contents = list(
        list(
          role = "user",
          parts = list(
            list(text = txt),
            list(text = prompt)
          )
        )
      ),
      generationConfig = list(
        temperature = 0.0
      )
    ),
    httr::timeout(1e6),
    encode = "json"
  ) |>
    httr::content()

  result |>
    purrr::pluck("candidates", 1, "content", "parts", 1, "text")
}

txt <- results_clusters |>
  purrr::list_rbind() |>
  dplyr::filter(decreto == unique(decreto)[1]) |>
  dplyr::filter(cluster_kmeans == 1) |>
  with(paste(artigo, collapse = "\n\n**************\n\n"))

analise_gemini_kmeans <- results_clusters |>
  purrr::list_rbind() |>
  dplyr::group_by(decreto, cluster_kmeans) |>
  dplyr::summarise(
    txt = paste(artigo, collapse = "\n\n**************\n\n"),
    .groups = "drop"
  ) |>
  dplyr::mutate(analise_gemini = purrr::map_chr(
    txt, \(x) pegar_infos_gemini(x, prompt), .progress = TRUE
  ))

analise_gemini_ward <- results_clusters |>
  purrr::list_rbind() |>
  dplyr::group_by(decreto, cluster_ward) |>
  dplyr::summarise(
    txt = paste(artigo, collapse = "\n\n**************\n\n"),
    .groups = "drop"
  ) |>
  dplyr::mutate(analise_gemini = purrr::map_chr(
    txt, \(x) pegar_infos_gemini(x, prompt), .progress = TRUE
  ))

readr::write_rds(analise_gemini_kmeans, "data-raw/analise_gemini_kmeans.rds")
readr::write_rds(analise_gemini_ward, "data-raw/analise_gemini_ward.rds")

prompt <- readr::read_file("data-raw/prompt_analise.md")

txt_codigo <- readr::read_file("data-raw/decretos.R")
prompt_codigo <- readr::read_file("data-raw/prompt_introducao.md")
analise_codigo <- pegar_infos_gemini(txt_codigo, prompt_codigo)

analise_codigo |>
  clipr::write_clip()


analise_gemini_kmeans |>
  dplyr::mutate(
    analise_gemini = stringr::str_remove(analise_gemini, "^#+ "),
    analise_gemini = paste("### Cluster:", cluster_kmeans, analise_gemini)
  ) |>
  dplyr::group_by(decreto) |>
  dplyr::summarise(
    txt = paste("##", decreto[1], paste(analise_gemini, collapse = "\n\n"))
  ) |>
  with(paste(txt, collapse = "\n\n")) |>
  cat()


analise_gemini_ward |>
  dplyr::mutate(
    analise_gemini = stringr::str_remove(analise_gemini, "^#+ "),
    analise_gemini = paste("### Cluster:", cluster_ward, analise_gemini)
  ) |>
  dplyr::group_by(decreto) |>
  dplyr::summarise(
    txt = paste("##", decreto[1], paste(analise_gemini, collapse = "\n\n"))
  ) |>
  with(paste(txt, collapse = "\n\n")) |>
  cat()











# --------------------------------------------------------------------------


da_decretos_cluster |>
  dplyr::filter(cluster_kmeans == 1) |>
  with(artigo) |>
  paste(collapse = "\n\n**************\n\n") |>
  cat()

da_decretos_cluster |>
  dplyr::count(cluster_kmeans)

da_decretos_cluster |>
  dplyr::filter(cluster_kmeans == 1)


da_decretos_cluster |>
  dplyr::count(cluster_ward)











stopw <- stopwords::stopwords("pt")

tidy_2gram <- da_decretos |>
  tidytext::unnest_tokens(word, artigo, token = "ngrams", n = 2) |>
  dplyr::filter(!stringr::str_detect(
    word,
    "[0-9]|qual é|julho de|lei n|trata se|se de|dois|decreto"
  )) |>
  dplyr::filter(!word %in% stopw)

dtm_list <- tidy_2gram |>
  dplyr::count(decreto, id_artigo, word) |>
  dplyr::group_split(decreto) |>
  purrr::set_names(unique(tidy_2gram$decreto)) |>
  purrr::map(\(x) tidytext::cast_dtm(x, decreto, word, value = n))

#install.packages("topicmodels")
lda_list <- dtm_list |>
  purrr::map(\(x) topicmodels::LDA(x, k = 4, control = list(seed = 123)))

plota_topico <- function(ap_topics, nm) {
  ap_top_terms <- ap_topics |>
    tidytext::tidy(matrix = "beta") |>
    dplyr::group_by(topic) |>
    dplyr::slice_max(beta, n = 10) |>
    dplyr::ungroup() |>
    dplyr::arrange(topic, -beta)
  ap_top_terms |>
    dplyr::mutate(term = tidytext::reorder_within(term, beta, topic)) |>
    ggplot2::ggplot(ggplot2::aes(beta, term, fill = factor(topic))) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::facet_wrap(~ topic, scales = "free", nrow = 1) +
    tidytext::scale_y_reordered() +
    ggplot2::labs(title = basename(tools::file_path_sans_ext(nm)))
}

purrr::imap(lda_list, plota_topico) |>
  patchwork::wrap_plots(ncol = 1)

