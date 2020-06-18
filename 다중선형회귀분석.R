# 자료 받아오기

car_data <- read.csv("car data.csv",
                     header = T)

head(car_data)

# factor값들을 숫자로 변경
car_data$Fuel_Type <- factor(car_data$Fuel_Type,
                             labels = c(0,1,2),
                             levels = c("CNG","Petrol","Diesel"))
car_data$Seller_Type <- factor(car_data$Seller_Type,
                               levels = c("Dealer","Individual"),
                               labels = c(0,1))
car_data$Transmission <- factor(car_data$Transmission, 
                                levels = c("Automatic", "Manual"),
                                labels = c(0,1))

head(car_data)


# 다중회귀분석 변수 선택(최상의 부분집합 선택)
car_data <- car_data[-1]
car_lm <- lm(Present_Price~., data=car_data)
summary(car_lm)


car_step <- step(car_lm, direction = "both")

car_step
summary(car_step)
car_step

car_lm2 <- lm(Present_Price~Year + Selling_Price , data= car_data)
summary(car_lm2)


# 잔차의 정규성 검정 1. 히스토그램으로 확인
redi_car <- car_lm2$residuals
hist(redi_car)


# qq플롯으로 확인 대각선모양이면 정규분포를따름.
qqnorm(redi_car)
qqline(redi_car, col='red') #선을 따라서 점이 찍힐 수록 정규분포를 따른다.

# Plot으로 확인

plot(car_step)



#shapiro 검정

shapiro.test(redi_car) #귀무가설: 데이터가 정규분포를 따른다. -> p-value가 0.05보다 크면 정규분포




# 패키지이용
#install.packages("car")
library("car")
scatterplotMatrix(car_data)


# 잔차의 등분산성 검정
# 귀무가설은 모든 잔차의 분산이 같다.

summary(model_5)
plot(resi_5)

residualPlots(car_step)

# 등분산검정
ncvTest(car_lm2) #비 상수성 테스트
#p_value 가 0.05보다 커야 잔차의 등 분산성(상수성)을 만족한다.






