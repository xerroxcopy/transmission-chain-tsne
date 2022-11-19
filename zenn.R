library(umap)
library(tidyverse)
library(snedata) # install via remotes::install_github("jlmelville/snedata")
library(imager) # for images
library(purrr)
library(Rtsne)  # https://github.com/jkrijthe/Rtsne

df_image <- 
  paste0(
    "input/drawing/2-1_", 
    formatC(0:41, width = 3, flag = "0"),
    ".jpg"
  ) |> # すべてのパスをstringのベクトルとして書き下す。フォルダ内に他ファイルがなければ`list.files()`でも可
  map(~ load.image(file = .x)) |> # imagerをpurrrのmapに渡して一気にリストとして読み込む
  map(~ resize(im = .x, size_x = 128, size_y = 128)) |> # 受け取った画像のリストをリサイズする。800*800くらいあったので…。
  map(~ grayscale(im = .x)) |> # もともとグレイスケールの画像だったがファイルとしては3チャンネルあったので削減
  map(~t(as.numeric(.))) |> # ここでまた数値に変換しないといけない。なぜ
  map_dfr(~ as.data.frame(.)) # このままではマトリックスなのでデータフレームに変換してからそれらを縦にひっつける(`map_dfr`)




image_umap <- df_image |> 
  umap(verbose = TRUE, n_threads = 6)
image_umap_df <- image_umap$layout |> 
  as_tibble() |> 
  transmute(x = V1, y = V2) |> 
  mutate(t = 0:41)

image_umap_df |> 
  ggplot(aes(x = x, y = y, colour = t)) +
  geom_point(size = .3) +
  geom_path() +
  geom_text(aes(label = t), hjust = 0, nudge_x = .1, size = 2) +
  scale_colour_viridis_c() +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line())
ggsave("./output/image_umap.png", height = 1000, width = 1500, unit = "px")

image_tsne <- df_image |>
  as.matrix() |> 
  Rtsne(
    perplexity = 12, # default 30, max datasize/3
    n_threads = 4,
    verbose = TRUE
  )
image_tsne_df <- image_tsne$Y |> 
  as_tibble() |> 
  rename(x = "V1", y = "V2") |> 
  mutate(t = 0:41)
image_tsne_df |> 
  ggplot(aes(x = x, y = y, colour = t)) +
  geom_point(size = .3) +
  geom_path() +
  geom_text(aes(label = t), hjust = 0, nudge_x = .1, size = 2) +
  scale_colour_viridis_c() +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line())
ggsave("./output/image_tsne.png", height = 1000, width = 1500, unit = "px")



nested_df_grid_conditions <- 
  expand_grid(
    min_dist = c(2^c(-7, -5, -3, -1)), 
    n_neighbors = c(4, 8, 16, 32)
  ) |> 
  mutate(
    umap = map2(
      min_dist, 
      n_neighbors,
      ~ umap(
        df_image,
        spread = 2,
        n_threads = 4, 
        min_dist = .x,
        n_neighbors = .y
      )
    )
  )
nested_df_grid_conditions
df_unnested_matrix_column <- 
  nested_df_grid_conditions |> 
  rowwise() |> 
  mutate(umap_df = umap |> extract2(1) |> list(),
         umap = NULL) |> 
  ungroup() |> 
  unnest_longer(umap_df, indices_to = "t")
df_unnested_matrix_column

df_matrix_only <- 
  df_unnested_matrix_column |> 
  select(umap_df) |> 
  deframe() |> 
  as_tibble()
df_grid_conditions <- 
  bind_cols(
    df_unnested_matrix_column, 
    df_matrix_only
  ) |> 
  select(-umap_df) |> 
  rename(x = "V1", y = "V2")

df_grid_conditions |> colnames()
df_grid_conditions |> 
  ggplot(aes(x = x, y = y, colour = t)) +
  geom_point(size = .3) + 
  geom_path(size = .1) +
  scale_colour_viridis_c() +
  facet_grid(rows = vars(min_dist), cols = vars(n_neighbors)) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line())
ggsave("./output/image_umap_grid_conditions.png", height = 1000, width = 1500, unit = "px")
