# 11/1/2026
# sierra raby
# bigfoot scrape for 2026 30DayMapChallenge day 1: points

# setup ----------------------------------------------------------
library(rvest)  
library(httr)     
library(dplyr)   
library(purrr)   
library(stringr)  
library(tibble)  

index_url <- "https://bfro.net/GDB/"   # starting page
cache_dir <- "bfro_page_cache" # avoid redownloading html
dir.create(cache_dir, showWarnings = FALSE)

# scrape fx --------------------------------------------------
fetch_html <- function(url, delay = 1.5, tries = 3) {
  f <- file.path(cache_dir, paste0(gsub("[^A-Za-z0-9]", "_", url), ".html"))
  if (file.exists(f) && file.size(f) > 500)
    return(paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
  for (i in seq_len(tries)) {
    Sys.sleep(delay * i)
    resp <- tryCatch(
      GET(url, user_agent("Scraper for the November 30DayMapChallenge (contact: sierra.raby@gmail.com)"), timeout(30)),
      error = function(e) { message("Request error: ", conditionMessage(e)); NULL }
    )
    if (!is.null(resp) && status_code(resp) == 200) {
      raw_txt <- rawToChar(content(resp, as = "raw"))          
      txt <- if (validUTF8(raw_txt)) raw_txt                  
      else iconv(raw_txt, from = "windows-1252", to = "UTF-8", sub = "") 
      if (!is.na(txt) && nchar(txt) > 500) {     # only save real pages
        writeLines(txt, f, useBytes = TRUE)     
        return(txt)
      }
    }
    message("Try ", i, " failed for ", url,
            if (!is.null(resp)) paste0(" (status ", status_code(resp), ")"))
  }
  NA_character_
}

# get all links from a page -----------------------------------------
get_links <- function(html_txt, current_url) {
  if (is.na(html_txt)) return(character(0))
  read_html(html_txt) %>%
    html_elements("a") %>%
    html_attr("href") %>%
    na.omit() %>%
    map_chr(~ xml2::url_absolute(.x, current_url)) %>% # now normalize
    str_replace("^http://", "https://") %>%       
    str_replace("^https://www\\.", "https://") %>%
    str_replace("/gdb/", "/GDB/") %>%             
    str_remove("#.*$") %>%  
    unique()
}

# URL patterns
# state and county pages
nav_pattern <- "(state_listing|show_county_reports)\\.asp\\?"
# individual report pages to get
report_pattern <- "show_report\\.asp\\?id=[0-9]+"

# crawler collecting report links - not scraping reports yet
crawl_reports <- function(start = index_url) {
  queue <- start   # queue pages to visit
  visited <- character(0) 
  reports <- character(0)    # list report URLs 
  while (length(queue) > 0) { 
    u <- queue[1]; queue <- queue[-1]  # get next page from queue
    if (u %in% visited) next  # no repeats
    visited <- c(visited, u)  
    message("Crawling: ", u)  
    links <- get_links(fetch_html(u), u) # get links from page to add to queue
    reports <- union(reports, str_subset(links, report_pattern))  # compile report links
    queue   <- union(queue, setdiff(str_subset(links, nav_pattern), visited))
  }
  # return report URLs 
  tibble(url = sort(reports),
         report_id = as.integer(str_extract(url, "(?<=id=)[0-9]+")))
}

# parse html -> row --------------------------------------
fields <- c("YEAR", "SEASON", "MONTH", "DATE", "STATE", "COUNTY",
            "LOCATION DETAILS", "NEAREST TOWN", "NEAREST ROAD",
            "OBSERVED", "ALSO NOTICED", "OTHER WITNESSES", "OTHER STORIES",
            "TIME AND CONDITIONS", "ENVIRONMENT")

parse_report <- function(url) {
  html_txt <- fetch_html(url)
  if (is.na(html_txt)) return(tibble(url = url, fetch_ok = FALSE))
  page <- read_html(html_txt, encoding = "UTF-8")
  txt  <- page %>% html_element("body") %>% html_text2()
  
  # label-based extraction
  label_re <- paste0("(?<=^|\\n)(", paste(fields, collapse = "|"), "):")
  pos <- str_locate_all(txt, regex(label_re))[[1]]
  vals <- setNames(rep(NA_character_, length(fields)), fields)
  if (nrow(pos) > 0) {
    labs <- str_match(str_sub(txt, pos[, 1], pos[, 2]), "^(.*):$")[, 2]
    ends <- c(pos[-1, 1] - 1, nchar(txt))
    for (i in seq_len(nrow(pos))) {
      vals[labs[i]] <- str_squish(str_sub(txt, pos[i, 2] + 1, ends[i]))
    }
  }
  
  # investigator follow-up
  env_parts <- str_split_fixed(vals[["ENVIRONMENT"]], "Follow-up investigation report by", 2)
  vals[["ENVIRONMENT"]] <- str_squish(env_parts[, 1])
  bio_parts <- str_split_fixed(env_parts[, 2], "About BFRO Investigator", 2)
  followup  <- str_squish(bio_parts[, 1])
  inv_bio   <- str_squish(bio_parts[, 2])
  
  # header info
  header    <- str_match(txt, "Report #\\s*([0-9]+)\\s*\\(Class\\s*([ABC])\\)")
  submitted <- str_match(txt, "Submitted by witness\\s*(.*?)\\s+on\\s+(.*?)\\.")
  # try for coords
  # lat <- str_match(html_txt, regex("lat(?:itude)?[^0-9-]{0,20}(-?[0-9]{1,2}\\.[0-9]+)", ignore_case = TRUE))[, 2]
  # lon <- str_match(html_txt, regex("lon(?:g|gitude)?[^0-9-]{0,20}(-?[0-9]{1,3}\\.[0-9]+)", ignore_case = TRUE))[, 2]
  
  bind_cols(
    tibble(url = url, fetch_ok = TRUE,
           report_id = as.integer(header[, 2]), class = header[, 3],
           submitted_by = submitted[, 2], submitted_date = submitted[, 3],
           # latitude = as.numeric(lat), longitude = as.numeric(lon),
           followup_report = followup, investigator_bio = inv_bio),
    as_tibble(as.list(vals)) %>% rename_with(~ tolower(str_replace_all(.x, " ", "_")))
  )
}

# test one report ----------------------------------------------------
test <- parse_report("https://www.bfro.net/GDB/show_report.asp?id=50449")  
print(as.data.frame(t(test)))  

# full run ----------------------
report_index <- crawl_reports() # crawl to list report URLs
saveRDS(report_index, "bfro_report_index.rds")   # save -> no recrawl

# parse reports
results <- map_dfr(report_index$url, possibly(parse_report, otherwise = NULL),
                   .progress = TRUE)
saveRDS(results, "bfro_reports.rds")                      
write.csv(results, "bfro_reports.csv", row.names = FALSE) 

# clean for mapping ----------------------------------
# add county fip
library(tigris)

# clean county names
norm_county <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("\\bst\\b\\.?", "saint") %>%
    str_remove_all("\\b(county|parish|borough|census area|municipality)\\b") %>%
    str_remove_all("[^a-z ]") %>%   
    str_squish()
}

# lookup table from tigris for FIPS
fips_lookup <- tigris::fips_codes %>%
  transmute(state_name,
            county_key = norm_county(county),
            fips = paste0(state_code, county_code))  

# join to reports
results_fips <- results %>%
  select(year,state, county) %>%
  mutate(county_key = norm_county(county)) %>%
  left_join(fips_lookup, by = c("state" = "state_name", "county_key")) %>%
  filter(!is.na(fips))

saveRDS(results_fips, "bfro_reports_fips.rds")


# clean years
results_clean <- results_fips %>%
  mutate(
    year_raw   = year,                                      
    year_first = str_extract(year_raw, "(?<!\\d)(\\d{4}|\\d{2})(?!\\d)"),  
    year_clean = case_when(
      is.na(year_first)      ~ NA_integer_,
      nchar(year_first) == 2 ~ as.integer(paste0("19", year_first)),  
      TRUE                   ~ as.integer(year_first)
    )
  ) %>%
  filter(between(year_clean, 1900, 2026)) %>%
  select(fips, year_clean)

# set up maps ------------------------------------------
library(sf)
options(tigris_use_cache = TRUE)

# records per county per year
counts <- results_clean %>%
  count(fips, year_clean, name = "n")

# county polygons 
counties <- tigris::counties(cb = TRUE, resolution = "5m") %>%
  shift_geometry() %>%
  select(fips = GEOID)

# FIPS not in tigris counties
setdiff(counts$fips, counties$fips)

# random points inside each county
cnt_sf <- counts %>% inner_join(counties, by = "fips") %>% st_as_sf()
set.seed(42)    
sample_one <- function(i) {
  p <- st_sample(st_geometry(cnt_sf)[i], size = cnt_sf$n[i], type = "random")
  st_sf(tibble(fips = rep(cnt_sf$fips[i], length(p)),
               year = rep(cnt_sf$year_clean[i], length(p))),
        geometry = p)
}
pts <- map(seq_len(nrow(cnt_sf)), sample_one) %>% bind_rows()

nrow(pts); nrow(results_clean)  
saveRDS(pts, "bfro_points.rds")

# test frame
library(ggplot2)

# state outlines
states <- tigris::states(cb = TRUE, resolution = "5m") %>%
  filter(!STATEFP %in% c("60", "66", "69", "72", "78")) %>%
  shift_geometry()

plot_year <- function(y, keep = 5, fade = 5, floor_alpha = 0) {
  d <- pts %>%
    filter(year <= y, year > y - keep) %>%
    mutate(a = pmax(floor_alpha, 1 - (y - year) / fade))
  
  ggplot() +
    geom_sf(data = states, fill = "#d0c3b8", color = "#40352a", linewidth = 0.2) +
    geom_sf(data = d, aes(alpha = a), size = 3, color = "#ff0800") +
    scale_alpha_identity() +
    labs(title = y) +
    theme_void() +
    theme(plot.background = element_rect(fill = "#40352a", color = NA),
          plot.title      = element_text(color = "white", size = 20, hjust = 0.5))
}

# set up mp4 ----------------------------------
# save map images
dir.create("frames", showWarnings = FALSE)
years <- 1970:max(pts$year)  

for (i in seq_along(years)) {
  ggsave(sprintf("frames/frame_%03d.png", i),  # 001, 002, ...
         plot_year(years[i]),
         width = 1920, height = 1080, units = "px", dpi = 150,
         bg = "#40352a")                            
}

library(av)

av_encode_video(list.files("frames", full.names = TRUE),
                output = "bfro_timeline.mp4",
                framerate = 6)   

