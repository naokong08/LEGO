
if (!require(pacman)) install.packages('pacman'); library(pacman)
pacman::p_load("chromote", "tidyverse", "stringr", "rvest", "dplyr", "xml2". "jsonlite")


try(b$close(), silent=TRUE)
try(system("taskkill /F /IM chrome.exe /T"), silent = TRUE)

args <- c(
  "--disable-blink-features=AutomationControlled",
  "--start-maximized",
  "--no-sandbox", 
  "--disable-infobars",
  "--user-agent=Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
)

browser_proc <- Chrome$new(args = args)
conn <- Chromote$new(browser = browser_proc)
b <- conn$new_session()

b$Network$enable()
b$Network$setBlockedURLs(urls = c("*.jpg", "*.jpeg", "*.png", "*.gif", "*.webp", "*.svg", "*.mp4", "*.woff", "*.css"))
b$Runtime$evaluate("Object.defineProperty(navigator, 'webdriver', {get: () => undefined})")


# 2. 대기 함수
wait_for_selector <- function(selector, timeout = 10) {
  start_time <- Sys.time()
  while (as.numeric(Sys.time() - start_time) < timeout) {
    exists <- b$Runtime$evaluate(sprintf("document.querySelector('%s') !== null", selector))$result$value
    if (isTRUE(exists)) return(TRUE)
    Sys.sleep(0.2)
  }
  return(FALSE)
}


# 3. 데이터 추출 함수
extract_lego_data <- function(url) {
  
  # 1. 초기화
  b$Page$navigate("about:blank")
  
  cat(paste0("🚀 접속: ", url, " ... "))
  b$Page$navigate(url, wait_ = FALSE)
  
  # 2. 로딩 대기
  if (!wait_for_selector('[data-test="product-price"]', timeout = 15)) {
    cat("❌ 로딩 실패 (시간 초과)\n")
    stop("Timeout") # 에러 발생시켜서 tryCatch로 보냄
  }
  
  # 3. 상세정보 클릭
  b$Runtime$evaluate("
    (function() {
      const btn = document.querySelector('button[data-test=\"pdp-specifications-accordion-title\"]');
      if(btn) { 
        btn.scrollIntoView({block: 'center'});
        btn.click(); 
      }
    })()
  ")
  
  # 4. 텍스트 로딩 대기
  for(k in 1:8) {
    chk <- b$Runtime$evaluate("document.body.innerText.includes('제조국')")$result$value
    if(isTRUE(chk)) break
    Sys.sleep(0.5)
  }
  
  # 5. JS 데이터 추출
  js_code <- "
    (function() {
      function t(s) { const e = document.querySelector(s); return e ? e.innerText.trim() : 'NA'; }
      
      function g(k) { 
        const items = Array.from(document.querySelectorAll('li'));
        const f = items.find(li => li.innerText.includes(k)); 
        if (f) {
           let txt = f.innerText;
           if(txt.includes(':')) {
              return txt.split(':')[1].trim();
           }
           return txt.replace(k, '').trim();
        }
        return '정보없음'; 
      }
      
      return {
        name: t('h1'), 
        code: t('[data-test=\"item-value\"]'), 
        price: t('[data-test=\"product-price\"]'),
        age: t('[data-test=\"ages-value\"]'), 
        pieces: t('[data-test=\"pieces-value\"]'),
        
        origin: g('제조국'), 
        date: g('출시년월'), 
        material: g('재질'), 
        size: g('크기 및 중량')
      };
    })();
  "
  
  result <- b$Runtime$evaluate(js_code, returnByValue = TRUE)$result$value
  
  if (result$name == "www.lego.com" || result$name == "NA") {
    cat("⚠️ 리다이렉트됨\n")
    stop("Redirected") # 에러 발생시켜서 tryCatch로 보냄
  }
  
  cat("성공! ✅\n")
  return(as_tibble(result))
}


# 4. 실행 (CSV 읽어서 전체 돌리기 - 재시도 기능 추가됨)

# CSV 파일 로드
product_links <- read.csv("product_links.csv")
product_links$category %>% unique %>% length()

dvdv <- function(i){
  
  target_urls <- product_links %>% 
    filter(category == unique(product_links$category)[i]) %>% 
    select(link) %>%
    unlist()
  
  results_list <- list()
  
  for (j in seq_along(target_urls)) {
    # 진행 상황 표시
    cat(paste0("[", j, "/", length(target_urls), "] "))
    
    # [수정됨] 3회 재시도 로직 (Retry Loop)
    success <- FALSE
    for (attempt in 1:5) {
      tryCatch({
        # 성공하면 루프 탈출
        results_list[[j]] <- extract_lego_data(target_urls[j])
        success <- TRUE
        break 
      }, error = function(e) {
        cat(paste0("⚠️ 에러(", attempt, "/5): ", e$message, " -> 재시도...\n"))
        Sys.sleep(3) # 잠시 대기 후 재시도
        
        # 만약 브라우저가 죽었으면 살려내기 (안전장치)
        try({
          b$Page$navigate("about:blank") 
        }, silent = TRUE)
      })
    }
    
    # 3번 다 실패했을 경우 빈 데이터 채우기 (멈춤 방지)
    if (!success) {
      cat("💀 최종 실패. 다음으로 건너뜁니다.\n")
      results_list[[j]] <- tibble(
        name="Fail", code=NA, price=NA, age=NA, pieces=NA, 
        origin=NA, date=NA, material=NA, size=NA
      )
    }
  }
  
  final_df <- bind_rows(results_list)
  return(final_df)
}



###########################################################################
# 실행 파트
###########################################################################
final_list <- list()

# 오류 방지를 위해 나눠서 실행
final_list[[1]] <- dvdv(1)
final_list[[2]] <- dvdv(2)
final_list[[3]] <- dvdv(3)
final_list[[4]] <- dvdv(4)
final_list[[5]] <- dvdv(5)
final_list[[6]] <- dvdv(6)

for (i in 7:17){
  final_list[[i]] <- dvdv(i)
}

for (i in 18:30){
  final_list[[i]] <- dvdv(i)
}
for (i in 31:42){
  final_list[[i]] <- dvdv(i)
}

final_list[[43]] <- dvdv(43)


tbt <- bind_rows(final_list)
tbt$category <- product_links$category

final_df <- tbt %>%
  mutate(across(everything(), ~ na_if(., "정보없음"))) %>%
  

  mutate(
    code = parse_number(code),   
    price = parse_number(price), 
    age = parse_number(age),     
    pieces = parse_number(pieces) 
  ) %>%
  separate(date, into = c("year", "month"), sep = "\\.", remove = TRUE) %>%
  
  mutate(
    year = as.numeric(year),
    month = as.numeric(month)
  ) %>%
  extract(
    size, 
    into = c("length", "width", "height", "weight"), 
    regex = "([0-9.]+)x([0-9.]+)x([0-9.]+)cm/([0-9.]+)", 
    remove = TRUE,
    convert = TRUE  # 자동으로 숫자형으로 변환
  ) %>%
  select(-material) %>%
  mutate(
    # origin에 "다양한 제조국"이 있으면 NA, 아니면 원래 값 유지
    origin = ifelse(str_detect(origin, "다양한 제조국"), 'various', origin)
  )

# 제품상세정보에 연도가 없는 제품 추가로 크롤링
codes_NAyear <- final_df %>% filter(is.na(year)) %>% select(code) %>% unlist
links_NAyear <- paste0("https://www.lego.com/ko-kr/service/building-instructions/",codes_NAyear )

get_years <- function(link){    
  read_html(link) %>% 
    html_element(xpath = "//p[contains(text(), '연도:')]") %>%
    html_text(trim = TRUE) %>%
    str_extract("\\d{4}") %>%
    return()
}

results <- sapply(links_NAyear, get_years)

write.csv(final_df, 'LEGO3.csv',  row.names = F)








