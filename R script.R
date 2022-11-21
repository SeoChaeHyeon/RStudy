# 패키지 설치 및 불러오기
install.packages('tidyverse') # dplyr, tidy,r ggplot2 등 tidy 패키지 생태계에 속하는 핵심 패키지들을 한번에 설치 및 관리
install.packages('data.table') # 대용량의 데이터에 대한 빠른 집계
install.packages('scales') # 그래프 축 및 범례 구분
install.packages('Rmisc') # multiplot

library(tidyverse)
library(data.table)
library(scales)
library(Rmisc)

# 데이터 셋 불러오기
orders <- read.csv("C:/Users/user/Desktop/발표/instacart-market-basket-analysis/orders.csv", fileEncoding = "UTF-8")
order_products_prior <- read.csv("C:/Users/user/Desktop/발표/instacart-market-basket-analysis/order_products__prior.csv", fileEncoding = "UTF-8")
order_products_train <- read.csv("C:/Users/user/Desktop/발표/instacart-market-basket-analysis/order_products__train.csv", fileEncoding = "UTF-8")
products <- read.csv("C:/Users/user/Desktop/발표/instacart-market-basket-analysis/products.csv", fileEncoding = "UTF-8")
aisles <- read.csv("C:/Users/user/Desktop/발표/instacart-market-basket-analysis/aisles.csv", fileEncoding = "UTF-8")
departments <- read.csv("C:/Users/user/Desktop/발표/instacart-market-basket-analysis/departments.csv", fileEncoding = "UTF-8")


table(orders$eval_set) # table(): 데이터의 빈도 확인, eval_set 컬럼값 출력
head(orders, 12)
glimpse(orders) # 데이터프레임의 구조 확인

head(order_products_prior, 10)
glimpse(order_products_prior)

head(order_products_train, 10)
glimpse(order_products_train)

head(products, 3)
glimpse(products)

head(aisles)
glimpse(aisles)

head(departments)
glimpse(departments)

# 자료형 변환
# dplyr의 기본 패키지의 기능을 사용하려면 :: 이중 콜론을 사용하여 지정해야 함(패키지이름::함수())
# mutate() 함수는 데이터 프레임에 변수를 추가할 때 사용
# as.numeric: data의 type을 numeric로 변환하는 함수, as.factor : data의 type을 factor로 변환하는 함수
orders <- dplyr::mutate(orders, eval_set = as.factor(eval_set), order_hour_of_day = as.numeric(order_hour_of_day)) 
# ex) 기존 eval_set 변수에 factor로 자료형을 변환한 후 생성
products <- dplyr::mutate(products, product_name = as.factor(product_name))
aisles <- dplyr::mutate(aisles, aisle = as.factor(aisle))
departments <- dplyr::mutate(departments, department = as.factor(department))


# order_products_prior(train set)과 order_products_train(validation set)을 합쳐서 데이터 탐색을 진행
order_products <- rbind(order_products_prior, order_products_train)
head(order_products)

# 고객 1명의 주문 횟수
# summarise()는 각종 통계함수와 함께 사용하여, 데이터프레임의 특정 변수에 속한 값들을 하나의 통계값으로 요약하여 반환하는 함수
# SQL에서 GROUP BY를 활용하여 각종 집계함수 결과를 만들어내는 것과 같은 작업을 R에서는 group_by()와 summarise()를 사용하여 구현
tmp <- orders %>% dplyr::group_by(user_id) %>% 
  dplyr::summarise(n_orders = last(order_number))

min(tmp$n_orders) # 4

# ggplot(사용할 데이터, aes(x축, y축 값 설정))
# geom_histogram(): 히스토그램, stat = "count": 데이터의 빈도 만큼
# scale_y_continuous(): labels, Y축 틱 레이블을 천 단위마다 콤마로 표시하기
ggplot(tmp, aes(x = n_orders)) + 
  geom_histogram(stat = "count", fill = "Sky Blue 3") + 
  scale_y_continuous(labels = comma)


# 주문 시간
# breaks 옵션은 축 눈금의 위치와 값을 조정
g1 <- ggplot(orders, aes(x = order_dow)) + 
  geom_histogram(stat = "count", fill = "Sky Blue 3") +
  scale_x_continuous(breaks = seq(0, 7, 1)) + # x좌표 0~7까지 1칸씩
  scale_y_continuous(breaks = seq(0, 700000, by=100000), labels = comma) # y좌표 0~700000까지 100000칸씩
# order_dow: 요일(0~6, 요일에 대한 정확한 정보가 없음)

g2 <- ggplot(orders, aes(x = order_hour_of_day)) + 
  geom_histogram(stat = "count", fill = "Sky Blue 3") +
  scale_x_continuous(breaks = seq(0, 25, 1)) +
  scale_y_continuous(breaks = seq(0, 400000, by=50000), labels = comma)
# order_hour_of_day: 주문한 시간대(0~24)

multiplot(g1, g2, layout = matrix(c(1,2), 2, 1, byrow = T)) # 레이아웃 설정

tmp <- orders %>% dplyr::group_by(order_dow, order_hour_of_day) %>% dplyr::summarise(Freq = n())
# n(): 빈도 수(개수 세기)

ggplot(tmp, aes(x = order_dow, y = order_hour_of_day, fill = Freq)) +
  geom_raster()+ # 히트맵 그리기
  scale_fill_gradient2()+ # 그라데이션으로 채우기
  scale_x_continuous(breaks = c(0:6)) + # x축 0~6칸
  scale_y_continuous(breaks = c(0:23)) + # y축 0~23칸
  theme(panel.background = element_blank()) + # 양 사이드 공백 없애기
  theme(axis.ticks = element_line(size = 1)) # axis.ticks = element_line(size = 1): 축을 따라 1씩 눈금 표시


# 재주문 기간
ggplot(orders, aes(x = days_since_prior_order)) + 
  geom_histogram(stat = "count", fill = "Sky Blue 3") +
  scale_x_continuous(breaks = seq(0, 40, 5)) + # x축 0~40칸 5씩
  scale_y_continuous(breaks = seq(0, 400000, by=50000), labels = comma) 
# y축 0~400000칸 50000씩, 1000단위 마다 콤마
# days_since_prior_order: 재주문하는데 걸린 기간(0~30)


# 1번의 주문당 구매 상품 개수
tmp <- order_products %>% dplyr::group_by(order_id) %>% 
  dplyr::summarise(n_products = last(add_to_cart_order))

tmp %>% dplyr::summarise(Min = min(n_products), Max = max(n_products)) # Min: 1, Max: 145

ggplot(tmp, aes(x = n_products)) + 
  geom_histogram(stat = "count", fill = "Sky Blue 3")+
  scale_x_continuous(breaks = seq(0, 80, 5))+ # x축 0~80칸 5씩
  scale_y_continuous(labels = comma)+ # 1000단위 마다 콤마
  coord_cartesian(xlim=c(0,80)) # x축 변경, 0~80까지만


# 가장 많이 팔린 상품
# dplyr::left_join(): x(왼쪽 테이블), y(오른쪽 테이블), by인자로 키 값 설정
# by=c(""="") 왼쪽엔 왼쪽 테이블의 키, 오른쪽엔 오른쪽 테이블의 키 지정
Bestseller <- order_products %>% dplyr::group_by(product_id) %>% 
  dplyr::summarise(Freq = n()) %>% # n(): 빈도 수, 요약
  dplyr::arrange(., desc(Freq)) # arrange(): 내림차순으로 정렬, 상품끼리 그룹지어 정렬
Bestseller <- dplyr::left_join(x = Bestseller, y = products, by = c("product_id" = "product_id"))

ggplot(Bestseller[1:15,], aes(x = reorder(product_name, -Freq), y = Freq)) + # 1~15개 품목
  geom_bar(stat = "identity", fill = "Sky Blue 3") + 
  # stat = "identity" 막대 그래프의 y축 높이를 지정한 데이터의 값
  geom_text(aes(label = Freq), position = position_stack(0.5), size = 2.5) + 
  # geom_text(aes( )): 그래프 안에 텍스트 넣기, geom_text(position= ): 글자 위치 지정
  scale_y_continuous(labels = comma)+ # 1000단위 마다 콤마
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  # theme(axis.text.x= ): x축의 테마 변경, element_text(): 축 제목의 텍스트 요소 부분
  # angle: 각도, vjust: 0=아래, 1=위, hjust: x축에 있는 글자 위치 조정
  xlab("Product") # x좌표 이름
