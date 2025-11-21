if (!require(pacman)) install.packages('pacman'); library(pacman)
pacman::p_load("chromote", "tidyverse", "stringr", "rvest", "dplyr", "xml2". "jsonlite")

html <- read_html("https://www.lego.com/ko-kr/themes") 

themes <- html %>%
  html_elements("a[class*='CategoryLeafstyles__ImagesLink']") %>%
  html_attr("href") %>%
  unique()

themes_links <- paste0("https://www.lego.com",themes)
themes_links

get_page_links <- function(baselink){
  
  json_text <- baselink %>%
    read_html() %>%
    html_element("#__NEXT_DATA__") %>%
    html_text()
  
  data <- fromJSON(json_text)
  
  # 4. 데이터 내부에서 '총 제품 수(total)'와 '페이지당 개수(perPage)' 찾기
  # Next.js의 Apollo State 구조 내에서 'ProductQueryResult'를 찾습니다.
  apollo_state <- data$props$pageProps$`__APOLLO_STATE__`
  
  # 키 이름이 매번 바뀌는 해시값(예: ProductQueryResult:abc12...)이므로 패턴으로 찾습니다.
  target_key <- grep("^ProductQueryResult", names(apollo_state), value = TRUE)[1]
  query_result <- apollo_state[[target_key]]
  
  total_products <- query_result$total    # 전체 제품 수 (예: 43)
  per_page <- query_result$perPage        # 페이지당 제품 수 (예: 18)
  
  # 5. 총 페이지 수 계산 (올림 처리)
  total_pages <- ceiling(total_products / per_page)
  
  links <- paste0(baselink,"?page=",1:total_pages)
  return(links)
}

get_product_links <- function(target_url){
  html <- read_html(target_url)
  json_text <- html %>% html_element("#__NEXT_DATA__") %>% html_text()
  data <- fromJSON(json_text)
  
  # 3. Apollo State 데이터 추출 (제품 정보가 들어있는 곳)
  apollo_state <- data$props$pageProps$`__APOLLO_STATE__`
  
  # 4. 'SingleVariantProduct'로 시작하는 키 찾기 (개별 제품 정보)
  # 데이터 내부에서 제품 정보를 담고 있는 객체들을 찾습니다.
  product_keys <- grep("^SingleVariantProduct:", names(apollo_state), value = TRUE)
  
  # 5. 각 제품 정보에서 'slug'(URL 뒷부분) 추출하여 링크 완성
  product_links <- c()
  
  for (key in product_keys) {
    item <- apollo_state[[key]]
    
    # slug가 있는 경우만 URL 생성 (예: "isabelles-house-visit-77049")
    if (!is.null(item$slug)) {
      full_link <- paste0("https://www.lego.com/ko-kr/product/", item$slug)
      product_links <- c(product_links, full_link)
    }
  }
  
  # 6. 중복 제거 및 결과 확인
  final_links <- unique(product_links)
  
  return(final_links)
}

themes_links %>% length

results_list <- list()

for (i in 1:length(themes_links)){
  # 2. 리스트의 i번째 칸에 티블 저장
  results_list[[i]] <- tibble(
    category = basename(themes_links[i]),
    link = unlist(map(get_page_links(themes_links[i]), get_product_links))
  )
}

# 3. 리스트를 하나의 티블로 결합
final_links <- bind_rows(results_list)

write.csv(final_links, 'product_links.csv', row.names = F)