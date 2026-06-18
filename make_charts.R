# SETUP --------------------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(plotly)
library(scales)
library(htmlwidgets)
library(glue)

options(scipen = 999)

# READ DATA ----------------------------------------------------------------------------
data_1 <- read_xlsx("chart_data.xlsx", sheet = "chart_1") |> 
  mutate(
    req_ed_level = fct_inorder(req_ed_level),
    post_pct = round(postings/sum(postings), 3)*100,
    post_label = paste0(round(postings/1000000, 1),"M"),
    post_pct_label = glue("{ifelse(post_pct >= 1, trunc(post_pct, 0), post_pct)}%")
  )

none_pct <- data_1 |> 
  filter(req_ed_level == "None") |> 
  select(post_pct_label) |> 
  deframe()

data_2 <- read_xlsx("chart_data.xlsx", sheet = "chart_2") |> 
  mutate(post_pct = postings/sum(postings), .by = occ_group)

none_ranks <- data_2 |> 
  filter(edu_req == "None") |> 
  mutate(none_rank = row_number(desc(post_pct))) |> 
  select(occ_group, none_rank)

data_2 <- data_2 |> 
  left_join(none_ranks, by = "occ_group") |> 
  mutate(
    post_pct_label = round(post_pct*100, 0),
    edu_req_label = if_else(edu_req == "None", "No credential required", "Any credential required")
  )

data_3 <- read_xlsx("chart_data.xlsx", sheet = "chart_3") |> 
  mutate(
    req_ed_level_combo = case_when(
      req_ed_level %in% c("Vocational/Technical", "Associate") ~ "Vocational/Technical, Certificate, Associate",
      req_ed_level %in% c("Master's", "Doctoral/Professional") ~ "Graduate Degree",
      .default = req_ed_level
    )
  ) |> 
  summarize(value = sum(value), .by = c(type, req_ed_level_combo)) |> 
  mutate(
    pct = value/sum(value)*100, .by = type,
    pct_label = glue("{ifelse(pct >= 1, round(pct, 0), round(pct,1))}%")
  )

# CHART 1: Postings by Education -------------------------------------------------------
chart_1 <- data_1 |> 
  ggplot(aes(
    x = postings, 
    y = fct_rev(req_ed_level),
    text = glue("<b>Postings:</b> {post_label} ({post_pct_label})")
  )) +
  geom_col(fill = "#2C5384", width = 0.8) +
  theme_classic() +
  theme(
    axis.line.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.y = element_line(linewidth = 0.5),
    axis.text = element_text(size = 11),
    plot.title = element_text(face = "bold", size = 14)
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = glue("Most Job Postings ({none_pct}) Do Not Specify any Credential Requirements"),
    x = NULL,
    y = NULL,
    caption = "<i>Source:</i> NLx Research Hub, Parsed Data Pilot"
  )


chart_1_interactive <- chart_1 |> 
  ggplotly(tooltip = "text") |> 
  layout(
    hoverlabel = list(
      bgcolor = "white"
    ),
    title = list(
      x = 0, y = 0.98,
      xref = "container", yref = "container",
      xanchor = "left", yanchor = "top"
    ),
    annotations = list(
      x = 1, y = 0, 
      text = "<i>Source</i>: NLx Research Hub, Parsed Data Pilot", 
      showarrow = FALSE, xref = 'paper', yref = 'paper', 
      xanchor = 'right', yanchor = 'bottom', 
      font = list(size = 11, color = "gray")
    ),
    xaxis = list(fixedrange = TRUE),
    yaxis = list(fixedrange = TRUE)
  ) |> 
  config(displayModeBar = FALSE)
 
saveWidget(chart_1_interactive, "chart_1.html", selfcontained = TRUE)


# CHART 2: No Requirements by Occupation Group -----------------------------------------
chart_2_colors <- c("Any" = "#2C5384", "None" = "#00000000")

chart_2 <- data_2 |> 
  filter(none_rank <= 5 | none_rank >= max(none_rank)-4) |> 
  ggplot(aes(
    x = post_pct, 
    y = fct_reorder(occ_group, none_rank, .desc = TRUE), 
    fill = edu_req,
    text = glue("<b>Share of group postings ({edu_req_label}):</b> {post_pct_label}%")
  )) +
  geom_col(
    color = "#2C5384", 
    width = 0.8) +
  scale_y_discrete(labels = label_wrap(36)) +
  scale_fill_manual(values = chart_2_colors) +
  theme_classic() +
  theme(
    axis.line.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.y = element_line(linewidth = 0.5),
    axis.text = element_text(size = 11),
    plot.title = element_text(face = "bold", size = 14),
    plot.caption = element_text(size = 10),
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 12)
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Postings for service-based areas of the workforce<br> are less-likely to include education requirements",
    x = NULL,
    y = NULL,
    fill = "Credential Required"
  )


chart_2_interactive <- chart_2 |> 
  ggplotly(tooltip = "text") |> 
  layout(
    hoverlabel = list(
      bgcolor = "white"
    ),
    title = list(
      x = 0, y = 0.97,
      xref = "container", yref = "container",
      xanchor = "left", yanchor = "top"
    ),
    legend = list(
      itemclick = "toggleothers",
      itemdoubleclick = FALSE,
      titleclick = FALSE,
      titledoubleclick = FALSE,
      traceorder = "reversed"
    ),
    margin = list(
      t = 60,
      l = 320,
      b = 40
    ),
    annotations = list(
      list(
        x = 1, y = -0.04,
        text = "<i>Source</i>: NLx Research Hub, Parsed Data Pilot",
        showarrow = FALSE, xref = "container", yref = "paper",
        xanchor = "right", yanchor = "bottom",
        font = list(size = 11, color = "gray")
      ),
      list(
        x = -0.9, y = 0.04,
        text = "<i><b>Lowest</b> Shares of Postings with<br>No Credential Requirement</i>",
        showarrow = FALSE, xref = "x domain", yref = "paper",
        xanchor = "center", yanchor = "center",
        textangle = 270,
        font = list(size = 14)
      ),
      list(
        x = -0.9, y = 0.97,
        text = "<i><b>Highest</b> Shares of Postings with<br>No Credential Requirement</i>",
        showarrow = FALSE, xref = "x domain", yref = "paper",
        xanchor = "center", yanchor = "center",
        textangle = 270,
        font = list(size = 14)
      )
    ),
    # shapes = list(
    #   list(
    #     type = "line", 
    #     x0 = -0.9, x1 = 1, 
    #     y0 = 5.5, y1 = 5.5,
    #     xref = "paper", yref = "y",
    #     line = list(color = "red", width = 3)
    #   )
    # ),
    xaxis = list(fixedrange = TRUE),
    yaxis = list(fixedrange = TRUE)
  ) |> 
  config(displayModeBar = FALSE)

chart_2_interactive$x$layout$shapes <- list(
  list(
    type = "line",
    x0 = -0.9, x1 = 1,
    y0 = 5.5, y1 = 5.5,
    xref = "paper",
    yref = "y",
    line = list(color = "black", width = 1)
  )
)

saveWidget(chart_2_interactive, "chart_2.html", selfcontained = TRUE)


# CHART 3: ACS Comparison --------------------------------------------------------------
chart_3_colors <- c(
  "High School" = "#2C5384", 
  "Vocational/Technical, Certificate, Associate" = "#4891BC", 
  "Bachelor's" = "#FFBB41", 
  "Graduate Degree" = "#D1523B" 
)

chart_3 <- data_3 |> 
  ggplot(aes(
    x = pct, 
    y = type, 
    fill = fct_rev(fct_inorder(req_ed_level_combo)),
    text = pct_label
  )) +
  geom_col(width = 0.8) +
  scale_y_discrete() +
  scale_fill_manual(values = chart_3_colors) +
  theme_classic() +
  theme(
    axis.line.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.y = element_line(linewidth = 0.5),
    axis.text = element_text(size = 11),
    plot.title = element_text(face = "bold", size = 14),
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 12)
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "High School and Bachelor's degrees are over-represented in job posting requirements<br> compared to the educational attainment of working adults",
    x = NULL,
    y = NULL,
    fill = ""
  )

chart_3_interactive <- chart_3 |> 
  ggplotly(tooltip = "text") |> 
  layout(
    hoverlabel = list(
      bgcolor = "white"
    ),
    title = list(
      x = 0, y = 0.97,
      xref = "container", yref = "container",
      xanchor = "left", yanchor = "top"
    ),
    margin = list(
      t = 60
    ),
    annotations = list(
      x = 1, y = 0, 
      text = "<i>Sources</i>: NLx Research Hub, Parsed Data Pilot;<br> U.S. Census Bureau, 2024 American Community Survey (ACS) 1-year Estimates", 
      showarrow = FALSE, xref = 'paper', yref = 'paper', 
      xanchor = 'right', yanchor = 'bottom', 
      align = "right",
      font = list(size = 11, color = "gray")
    ),
    legend = list(
      orientation = "h",
      traceorder = "reversed",
      itemclick = FALSE,
      itemdoubleclick = FALSE
    ),
    xaxis = list(fixedrange = TRUE),
    yaxis = list(fixedrange = TRUE)
  ) |> 
  config(displayModeBar = FALSE)

saveWidget(chart_3_interactive, "chart_3.html", selfcontained = TRUE)
