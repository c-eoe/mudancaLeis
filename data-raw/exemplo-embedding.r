palavras_direito <- c(
  "sentença", "acórdão", "lei", "decreto"
)

r <- httr::POST(
  "https://api.openai.com/v1/embeddings",
  body = list(
    model = "text-embedding-3-large",
    input = palavras_direito,
    dimensions = 2000
  ),
  httr::add_headers("Authorization" = paste("Bearer", Sys.getenv("OPENAI_API_KEY"))),
  encode = "json"
)

embeds <- r |>
  httr::content(simplifyDataFrame = TRUE) |>
  purrr::pluck("data") |>
  tibble::as_tibble() |>
  tidyr::unnest(embedding) |>
  dplyr::select(-object) |>
  dplyr::transmute(
    name = palavras_direito[index + 1],
    value = embedding
  ) |>
  dplyr::group_by(name) |>
  dplyr::mutate(id_embedding = seq_len(dplyr::n())) |>
  dplyr::ungroup() |>
  tidyr::pivot_wider() |>
  dplyr::select(-id_embedding)

writexl::write_xlsx(embeds, "data-raw/xlsx/embeds_example.xlsx")

p <- embeds |>
  M3C::tsne(
    labels = palavras_direito,
    perplex = 1,
    seed = 10,
    text = palavras_direito,
    textlabelsize = 0,
    dotsize = 5
  ) +
  ggplot2::geom_line(
    ggplot2::aes(group = c(1, 1, 2, 2)),
    linetype = 2
  ) +
  ggrepel::geom_text_repel(
    ggplot2::aes(label = label),
    size = 5
  ) +
  ggplot2::geom_point() +
  ggplot2::scale_colour_viridis_d(begin = .2, end = .8) +
  ggplot2::guides(colour = "none") +
  ggplot2::theme_bw(15) +
  ggplot2::labs(
    title = "",
    x = "", y = ""
  )

ggplot2::ggsave(
  "data-raw/svg/decretos_exemplo.svg",
  p, width = 6, height = 5, dpi = 600
)

viridis::viridis(4, begin = .2, end = .8)
