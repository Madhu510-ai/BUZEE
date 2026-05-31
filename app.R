# ==============================
# 📦 LOAD LIBRARIES
# ==============================
library(xml2)
library(jsonlite)
library(dplyr)
library(googlesheets4)
library(googledrive)




spreadsheet_url <- "1dn2rakTrBv2tFjgaXIjXauvQ6c-XVuZskN8oONku6YA"

drive_folder_id <- "1LV1i3c0DA23_SjeSOrpy2DiwvUs9USY1"


# ==============================
# 📰 GOOGLE RSS FUNCTION
# ==============================
get_news_rss <- function(stock) {
  
  stock_encoded <- URLencode(stock)
  
  url <- paste0("https://news.google.com/rss/search?q=", stock_encoded,
                "&hl=en-IN&gl=IN&ceid=IN:en")
  
  page <- tryCatch(read_xml(url), error = function(e) return(NULL))
  if (is.null(page)) return(data.frame())
  
  items <- xml_find_all(page, "//item")
  
  df <- data.frame(
    Title = xml_text(xml_find_all(items, "title")),
    Link  = xml_text(xml_find_all(items, "link")),
    Date  = as.Date(xml_text(xml_find_all(items, "pubDate")),
                    format = "%a, %d %b %Y"),
    Source = "RSS",
    stringsAsFactors = FALSE
  )
  
  df %>%
    filter(Title != "", !is.na(Date)) %>%
    distinct(Link, .keep_all = TRUE)
}

# ==============================
# 🌍 GDELT FUNCTION
# ==============================
get_news_gdelt <- function(stock) {
  
  url <- paste0(
    "https://api.gdeltproject.org/api/v2/doc/doc?query=",
    URLencode(stock),
    "&mode=ArtList&maxrecords=100&format=json"
  )
  
  data <- tryCatch(fromJSON(url), error = function(e) return(NULL))
  
  if (is.null(data) || is.null(data$articles)) return(data.frame())
  
  df <- data.frame(
    Title  = data$articles$title,
    Link   = data$articles$url,
    Date   = as.Date(substr(data$articles$seendate, 1, 8), "%Y%m%d"),
    Source = "GDELT",
    stringsAsFactors = FALSE
  )
  
  df %>%
    filter(Title != "", !is.na(Date)) %>%
    distinct(Link, .keep_all = TRUE) %>%
    filter(Date >= Sys.Date() - 365)
}

# ==============================
# 📊 STOCK LIST
# ==============================
# ==============================
# READ PORTFOLIO
# ==============================

portfolio_data <- read_sheet(
  ss = spreadsheet_url,
  sheet = "Portfolio"
)

stocks <- portfolio_data$stock

stocks <- stocks[
  !is.na(stocks)
]

stocks <- unique(stocks)

print("Stocks loaded from Portfolio sheet:")
print(stocks)

# ==============================
# 🔁 FETCH DATA
# ==============================
all_data <- data.frame()

for (stock in stocks) {
  
  print(paste("Fetching:", stock))
  
  rss_data   <- get_news_rss(stock)
  gdelt_data <- get_news_gdelt(stock)
  
  combined <- rbind(rss_data, gdelt_data)
  
  if (nrow(combined) > 0) {
    combined$Stock <- stock
    combined$Month <- format(combined$Date, "%Y-%m")
  }
  
  all_data <- rbind(all_data, combined)
}

# ==============================
# 🧹 CLEAN DATA
# ==============================
all_data <- all_data %>%
  distinct(Link, .keep_all = TRUE)

# ==============================
# 💾 SAVE RAW DATA CSV
# ==============================
# Keep local backup
write.csv(
  all_data,
  "stock_news_data.csv",
  row.names = FALSE
)

# Add collection date
all_data$Run_Date <- Sys.Date()

existing_news <- read_sheet(
  ss = spreadsheet_url,
  sheet = "NewsData"
)

combined_news <- bind_rows(
  existing_news,
  all_data
)

combined_news <- combined_news %>%
  distinct(
    Link,
    .keep_all = TRUE
  )

sheet_write(
  combined_news,
  ss = spreadsheet_url,
  sheet = "NewsData"
)

cat(
  "NewsData updated with only new unique news\n"
)

# ==============================
# ==============================
# 🧠 TREND SCORE CALCULATION
# ==============================
all_data$Date <- as.Date(all_data$Date)

all_data$Days_Old <- as.numeric(
  Sys.Date() - all_data$Date
)

all_data$Weight <- 1 / (1 + all_data$Days_Old)

trend_data <- all_data %>%
  group_by(Stock) %>%
  summarise(
    News_Count = n(),
    Trend_Score = sum(Weight)
  ) %>%
  arrange(desc(Trend_Score))

print(trend_data)

# ==============================
# USER PORTFOLIO ANALYTICS
# ==============================

all_users <- unique(portfolio_data$chat_id)

user_links <- data.frame()

# ==============================
# 💾 SAVE TREND CSV
# ==============================
# Local backup
write.csv(
  trend_data,
  "trend_score_data.csv",
  row.names = FALSE
)

# Overwrite latest trend score
sheet_write(
  trend_data,
  ss = spreadsheet_url,
  sheet = "TrendScore"
)

cat("TrendScore updated in Google Sheets\n")

# ==============================
# 📊 MONTHLY COUNTS
# ==============================
monthly_counts <- table(all_data$Month)

# ==============================
# 📊 CREATE DASHBOARD PNG
# ==============================
png(
  "D:/BUZEE/stock_dashboard.png",
  width = 2400,
  height = 900,
  res = 150
)

par(mfrow = c(1,3))

# ------------------------------
# GRAPH 1: NEWS FREQUENCY
# ------------------------------
par(mar = c(10,4,4,2))

barplot(
  table(all_data$Stock),
  col = c(
    "skyblue",
    "orange",
    "lightgreen",
    "pink",
    "yellow",
    "purple",
    "cyan",
    "red"
  ),
  main = "News Frequency by Stock",
  xlab = "Stock",
  ylab = "Articles",
  las = 2,
  cex.names = 0.8
)

# ------------------------------
# GRAPH 2: MONTHLY TREND
# ------------------------------
par(mar = c(10,4,4,2))

barplot(
  monthly_counts,
  col = "steelblue",
  main = "Monthly News Trend",
  xlab = "Month",
  ylab = "Articles",
  las = 2,
  cex.names = 0.8
)

# ------------------------------
# GRAPH 3: TREND SCORE
# ------------------------------
par(mar = c(10,4,4,2))

barplot(
  trend_data$Trend_Score,
  names.arg = trend_data$Stock,
  col = "darkgreen",
  main = "Trend Score by Stock",
  xlab = "Stock",
  ylab = "Trend Score",
  las = 2,
  cex.names = 0.8
)

mtext(
  "Stock News Analytics Dashboard",
  outer = TRUE,
  line = -2,
  cex = 2,
  font = 2
)

dev.off()

# ==============================
# UPLOAD DASHBOARD TO GOOGLE DRIVE
# ==============================
dashboard_file <- "D:/BUZEE/stock_dashboard.png"

# Delete old dashboard from folder
old_file <- drive_ls(as_id(drive_folder_id))
old_file <- old_file[old_file$name == "stock_dashboard.png", ]

if(nrow(old_file) > 0){
  drive_rm(old_file)
}

# Upload new dashboard
drive_upload(
  media = dashboard_file,
  path = as_id(drive_folder_id),
  name = "stock_dashboard.png"
)

cat("Dashboard uploaded to Google Drive\n")

uploaded <- drive_find(
  pattern = "stock_dashboard.png"
)

drive_share_anyone(uploaded)

cat("Dashboard shared publicly\n")

uploaded <- drive_find(
  pattern = "stock_dashboard.png"
)

dashboard_link <- uploaded$drive_resource[[1]]$webViewLink

cat("\nDashboard URL:\n")
print(dashboard_link)


# ==============================
# USER COMPARISON DATA
# ==============================

user_scores <- portfolio_data %>%
  
  left_join(
    trend_data,
    by = c("stock" = "Stock")
  ) %>%
  
  mutate(
    Trend_Score = ifelse(
      is.na(Trend_Score),
      0,
      Trend_Score
    )
  ) %>%
  
  group_by(chat_id) %>%
  
  summarise(
    Portfolio_Score = sum(Trend_Score)
  ) %>%
  
  arrange(
    desc(Portfolio_Score)
  )

# ==============================
# USER DASHBOARDS
# ==============================

print("===================================")
print("STARTING USER DASHBOARD GENERATION")
print("===================================")

print(all_users)

for(user_chat_id in all_users)
{
  
  tryCatch({
    
    cat("\n=================================\n")
    cat("Processing User:", user_chat_id, "\n")
    cat("=================================\n")
    
    # ==========================
    # USER PORTFOLIO
    # ==========================
    
    user_portfolio <- portfolio_data %>%
      filter(chat_id == user_chat_id)
    
    print(user_portfolio)
    
    # ==========================
    # JOIN TREND DATA
    # ==========================
    
    user_trends <- user_portfolio %>%
      left_join(
        trend_data,
        by = c("stock" = "Stock")
      ) %>%
      mutate(
        Trend_Score = ifelse(
          is.na(Trend_Score),
          0,
          Trend_Score
        )
      ) %>%
      arrange(desc(Trend_Score))
    
    print(user_trends)
    
    # ==========================
    # EMPTY CHECK
    # ==========================
    
    if(nrow(user_trends) == 0)
    {
      cat("No portfolio data found\n")
      next
    }
    
    # ==========================
    # TOTAL SCORE
    # ==========================
    
    total_score <- sum(user_trends$Trend_Score)
    
    cat("Total Score:", total_score, "\n")
    
    if(total_score == 0)
    {
      cat("All trend scores are zero. Skipping.\n")
      next
    }
    
    # ==========================
    # SHARE %
    # ==========================
    
    user_trends <- user_trends %>%
      mutate(
        Share = round(
          (Trend_Score / total_score) * 100,
          2
        )
      )
    
    print(user_trends)
    
    # ==========================
    # FILE NAME
    # ==========================
    
    file_name <- paste0(
      "D:/BUZEE/user_",
      user_chat_id,
      "_dashboard.png"
    )
    
    cat("Creating File:\n")
    print(file_name)
    
    # ==========================
    # CREATE DASHBOARD
    # ==========================
    
    png(
      filename = file_name,
      width = 1800,
      height = 900,
      res = 150
    )
    
    par(mfrow = c(1,2))
    
    # --------------------------
    # BAR CHART
    # --------------------------
    
    par(mar = c(10,4,4,2))
    
    barplot(
      user_trends$Trend_Score,
      names.arg = user_trends$stock,
      col = "darkgreen",
      las = 2,
      cex.names = 0.8,
      main = "Portfolio Trend Ranking",
      xlab = "Stocks",
      ylab = "Trend Score"
    )
    
    # --------------------------
    # USER COMPARISON CHART
    # --------------------------
    
    comparison_scores <- user_scores
    
    comparison_names <- ifelse(
      comparison_scores$chat_id == user_chat_id,
      paste0(
        "YOU (#",
        rank(
          -comparison_scores$Portfolio_Score,
          ties.method = "min"
        )[comparison_scores$chat_id == user_chat_id],
        ")"
      ),
      "Other User"
    )
    
    par(mar = c(5,10,4,2))
    
    barplot(
      comparison_scores$Portfolio_Score,
      
      names.arg = comparison_names,
      
      horiz = TRUE,
      
      las = 1,
      
      col = ifelse(
        comparison_scores$chat_id == user_chat_id,
        "red",
        "steelblue"
      ),,
      
      main = "Your Portfolio vs Other Users",
      
      xlab = "Portfolio Trend Score"
    )
    
    # --------------------------
    # DASHBOARD TITLE
    # --------------------------
    
    user_rank <- rank(
      -user_scores$Portfolio_Score,
      ties.method = "min"
    )[user_scores$chat_id == user_chat_id]
    
    user_score <- user_scores$Portfolio_Score[
      user_scores$chat_id == user_chat_id
    ]
    
    mtext(
      paste(
        "Portfolio Analytics -",
        user_chat_id,
        "| Rank:",
        user_rank,
        "/",
        nrow(user_scores),
        "| Score:",
        round(user_score, 2)
      ),
      outer = TRUE,
      cex = 1.3,
      font = 2
    )
    
    dev.off()
    
    cat("Dashboard Created Successfully\n")
    # ==========================
    # DELETE OLD FILE
    # ==========================
    
    old_file <- drive_ls(
      as_id(drive_folder_id)
    )
    
    old_file <- old_file[
      old_file$name ==
        basename(file_name),
    ]
    
    if(nrow(old_file) > 0)
    {
      drive_rm(old_file)
      cat("Old Dashboard Deleted\n")
    }
    
    # ==========================
    # UPLOAD TO DRIVE
    # ==========================
    
    uploaded_file <- drive_upload(
      media = file_name,
      path = as_id(drive_folder_id),
      name = basename(file_name)
    )
    
    drive_share_anyone(uploaded_file)
    
    drive_link <- uploaded_file$drive_resource[[1]]$webViewLink
    
    cat("Dashboard Uploaded\n")
    print(drive_link)
    
    # ==========================
    # STORE LINK
    # ==========================
    
    user_links <- rbind(
      user_links,
      data.frame(
        chat_id = as.character(user_chat_id),
        image_link = as.character(drive_link)
      )
    )
    
    cat(
      "Completed User:",
      user_chat_id,
      "\n"
    )
    
  }, error = function(e){
    
    cat("\n========================\n")
    cat("ERROR FOR USER:\n")
    cat(user_chat_id, "\n")
    cat("========================\n")
    
    print(e)
    
  })
}

# ==============================
# SAVE LINKS TO GOOGLE SHEET
# ==============================

print(user_links)

if(nrow(user_links) > 0)
{
  
  sheet_write(
    user_links,
    ss = spreadsheet_url,
    sheet = "UserDashboards"
  )
  
  cat(
    "\nUser Dashboard Links Saved\n"
  )
  
} else {
  
  cat(
    "\nNo Dashboards Generated\n"
  )
}

# ==============================
# SHOW DASHBOARD IN RSTUDIO
# ==============================

dev.new(width = 18, height = 7)

par(mfrow = c(1,3))

barplot(
  table(all_data$Stock),
  col = c(
    "skyblue",
    "orange",
    "lightgreen",
    "pink",
    "yellow",
    "purple",
    "cyan",
    "red"
  ),
  main = "News Frequency by Stock",
  las = 2,
  cex.names = 0.8
)

barplot(
  monthly_counts,
  col = "steelblue",
  main = "Monthly News Trend",
  las = 2,
  cex.names = 0.8
)

barplot(
  trend_data$Trend_Score,
  names.arg = trend_data$Stock,
  col = "darkgreen",
  main = "Trend Score by Stock",
  las = 2,
  cex.names = 0.8
)

# ==============================
# SUCCESS MESSAGE
# ==============================
cat("\n=================================\n")
cat("FILES GENERATED SUCCESSFULLY\n")
cat("=================================\n")
cat("stock_news_data.csv\n")
cat("trend_score_data.csv\n")
cat("D:/BUZEE/stock_dashboard.png\n")
cat("=================================\n")



