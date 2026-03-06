
library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(scales)
library(DT)

DATA <- readRDS(file.path("data","policy_data.rds"))
panel <- DATA$panel
latest <- DATA$latest_policy
b <- DATA$coef_inst10
se <- DATA$se_inst10

mmr_from_eta <- function(eta){ pmax(0, exp(eta) - 1) }

policy_translate <- function(base_mmr, delta_pp, beta, beta_se){
  eta0 <- log(base_mmr + 1)
  d_eta <- beta * (delta_pp/10)
  d_eta_lo <- (beta - 1.96*beta_se) * (delta_pp/10)
  d_eta_hi <- (beta + 1.96*beta_se) * (delta_pp/10)

  mmr_new <- mmr_from_eta(eta0 + d_eta)
  mmr_new_lo <- mmr_from_eta(eta0 + d_eta_lo)
  mmr_new_hi <- mmr_from_eta(eta0 + d_eta_hi)

  delta <- base_mmr - mmr_new
  delta_lo <- base_mmr - mmr_new_hi
  delta_hi <- base_mmr - mmr_new_lo

  pct <- 100 * (1 - (mmr_new + 1) / (base_mmr + 1))
  list(mmr_new = mmr_new, delta = delta, delta_lo = delta_lo, delta_hi = delta_hi, pct = pct)
}

theme_pub <- function(){
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face="bold", size=15, margin=margin(b=6)),
      plot.subtitle = element_text(size=11, margin=margin(b=8)),
      plot.caption = element_text(size=9, hjust=0, margin=margin(t=10)),
      axis.title = element_text(face="bold"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

ui <- page_navbar(
  title = "Facility Delivery Coverage & Maternal Mortality (Policy Explorer)",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  header = tags$head(
    tags$link(rel="stylesheet", type="text/css", href="style.css"),
    tags$script(src="custom.js")
  ),

  nav_panel(
    "Country Explorer",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("cc", "Select country", choices = sort(unique(latest$country_code))),
        sliderInput("delta", "Assumed increase in institutional delivery coverage (percentage points)",
                    min=0, max=30, value=10, step=1),
        helpText(class="small-note",
                 "Associational translation from a fixed-effects model; not causal proof.")
      ),
      card(
        card_header("Policy impact snapshot"),
        uiOutput("kpis"),
        tags$hr(class="soft"),
        plotOutput("ts", height = 320),
        tags$hr(class="soft"),
        DTOutput("country_table")
      )
    )
  ),

  nav_panel(
    "Global Evidence",
    layout_columns(
      col_widths = c(6,6),
      card(card_header("Dose–response shape (spline)"),
           tags$img(src="fig1_dose_response_spline.png", style="width:100%; border-radius:14px;")),
      card(card_header("Robustness across specifications"),
           tags$img(src="fig5_spec_robustness_forest.png", style="width:100%; border-radius:14px;"))
    ),
    layout_columns(
      col_widths = c(6,6),
      card(card_header("Placebo lead vs lag"),
           tags$img(src="fig2_placebo_lead_vs_lag.png", style="width:100%; border-radius:14px;")),
      card(card_header("Global trends"),
           tags$img(src="fig3_global_trends.png", style="width:100%; border-radius:14px;"))
    ),
    layout_columns(
      col_widths = c(12),
      card(card_header("Priority setting (top 25)"),
           tags$img(src="fig4_priority_countries_top25.png", style="width:100%; border-radius:14px;"))
    )
  ),

  nav_panel(
    "Download",
    card(
      card_header("Download policy table"),
      downloadButton("dl_table", "Download country policy table (CSV)")
    )
  )
)

server <- function(input, output, session){

  output$kpis <- renderUI({
    row <- latest %>% filter(country_code == input$cc) %>% slice(1)
    if(nrow(row)==0) return(NULL)
    base_mmr <- row$mmr
    base_cov <- row$inst_birth
    out <- policy_translate(base_mmr, input$delta, b, se)

    tagList(
      div(class="kpi",
          h4(sprintf("Baseline MMR: %.1f", base_mmr)),
          p(sprintf("Baseline institutional delivery: %.1f%%", base_cov))),
      div(style="height:10px;"),
      div(class="kpi",
          h4(sprintf("Projected MMR after +%dpp: %.1f", input$delta, out$mmr_new)),
          p(sprintf("Reduction: %.1f (95%% CI %.1f to %.1f) per 100,000 | %.2f%% (MMR+1)",
                    out$delta, out$delta_lo, out$delta_hi, out$pct)))
    )
  })

  output$ts <- renderPlot({
    df <- panel %>% filter(country_code == input$cc) %>% arrange(year)
    ggplot(df, aes(x=year)) +
      geom_line(aes(y=mmr), linewidth=1.05) +
      geom_line(aes(y=inst_birth * (max(mmr, na.rm=TRUE)/100)), linetype=2, linewidth=1.05) +
      coord_cartesian(clip="off") +
      labs(
        title = "Country time series",
        subtitle = "Solid: MMR; Dashed: institutional delivery (scaled to MMR axis)",
        x = "Year", y = "MMR (per 100,000)",
        caption = "Scaling is visual only."
      ) +
      scale_y_continuous(labels = label_number(big.mark=",")) +
      theme_pub()
  })

  output$country_table <- renderDT({
    df <- latest %>% filter(country_code == input$cc) %>%
      select(country_code, who_region, year, mmr, inst_birth,
             mmr_reduction10, pct_red10, mmr_reduction20, pct_red20)
    datatable(df, options = list(dom="t", scrollX = TRUE))
  })

  output$dl_table <- downloadHandler(
    filename = function(){ "country_policy_translation.csv" },
    content = function(file){
      write.csv(latest, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)

