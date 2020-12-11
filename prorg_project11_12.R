library('readxl')
library('dplyr')
library('purrr')
library('fastDummies')
library('tidyr')
library("corrplot")
library("AER")
library("sandwich")
library("lmtest")
library("stargazer")
library("ggplot2")
library('tidyverse')
library("scales")
#library("openintro")
#library("vars")
#library("het.test")

flats <- read_excel('all_flats_u.xlsx')


#удаляем ненужные переменные
flats <- select(flats, -c(type,adress,phone,square1,okna,sanuzel,
                           children, additional, ZHK,seria_doma, ssylka))

#превращаем число комнат в num
flats$number_of_rooms <- substr(flats$number_of_rooms, start = 1, stop = 1)

#превращаем площадь в num
flats$total_area <- as.numeric(map(strsplit(flats$square,split='/'), 1))
flats$square2 <- as.character(map(strsplit(flats$square,split='/'), 2))
flats$which_area <- ifelse(flats$square2=='NULL',0,as.numeric(flats$square2))
flats$living_area <- ifelse(flats$which_area/flats$total_area>=0.4,
                            flats$which_area/flats$total_area,NA)

plot(density(flats$living_area,na.rm = TRUE))


flats <- select(flats, -c(square2,which_area,square))



#создаем переменную "номер этажа"
flats$floor <- as.numeric(map(strsplit(flats$house,split="/"), 1))

#создаем переменную "общее число этажей в доме"
flats$total_floor <- as.numeric(
  map(strsplit(as.character(map(strsplit(flats$house,split="/"), 2)),split = ','),1))


#цену в num
flats$price <- as.numeric(map(strsplit(flats$price,split='руб'), 1))


# создаем бинарные переменные для района и ремонта
flats <- dummy_cols(flats, select_columns = 'rajon')
flats <- dummy_cols(flats, select_columns = 'repair')


#создаем бинарные для парковки, балкона и мусоропровода
flats$parking <- ifelse(flats$parking != 'NA', 1, 0)
flats$parking[is.na(flats$parking)] <- 0
flats$balkon <- ifelse(flats$balkon == 'NA', 0, 1)
flats$balkon[is.na(flats$balkon)] <- 0
flats$chute <- ifelse(flats$musoroprovod == 'Да', 1, 0)
flats$chute[is.na(flats$chute)] <- 0
flats$lift <- ifelse(flats$lift == 'NA', 0, 1)
flats$lift[is.na(flats$lift)] <- 0

#фича, равная расстоянию до метро в минутах
flats$metro_dist <-as.numeric(str_extract(
  sapply(strsplit(flats$metro, "\\s+"), function(x) paste(rev(x), collapse=" "))
  ,'\\d{1,2}'))
#пешком или на машине
flats$on_foot <- as.numeric(str_detect(flats$metro, regex("пешком", ignore_case = TRUE)))


#наличие дискриминационного объявления 
flats$discrim_text <- as.numeric(str_detect(flats$descrip, regex(" азии|кавказ|славян", ignore_case = TRUE)))

#риелторские агенства
flats$rieltor <- as.numeric(str_detect(flats$descrip, regex("лот |ID", ignore_case = TRUE)))

#тип постройки
flats$type <- map(strsplit(flats$house,split=", "), 2)

flats <- dummy_cols(flats, select_columns = 'type')

flats <- filter(flats, flats$type_NULL == 0)

flats <- select(flats,-c(ID, metro, house, descrip, repair, musoroprovod,
                         type, type_NULL))


flats$blochniy <- flats$type_Блочный
flats$kirpichniy <- flats$type_Кирпичный
flats$monolitno_kirpichniy <-  flats$`type_Монолитно-кирпичный`
flats$monolitniy <- flats$type_Монолитный
flats$panelniy <- flats$type_Панельный
flats$stalinskiy <- flats$type_Сталинский
flats$starii_fond <- flats$`type_старый фонд`


#c(flats$blochniy, flats$derevyann, flats$kirpichniy, flats$monolitno_kirpichniy,flats$monolitniy, flats$panelniy, flats$stalinskiy, flats$starii_fond)
flats <- select(flats, -c(type_Блочный, type_Деревянный, type_Кирпичный,
                          `type_Монолитно-кирпичный`, type_Монолитный,
                          type_Панельный, type_Сталинский, `type_старый фонд`
                          ))

#месторасположение
flats$VAO <- flats$rajon_ВАО
flats$ZAO <- flats$rajon_ЗАО
flats$ZelAO <- flats$rajon_ЗелАО
flats$SAO <- flats$rajon_САО
flats$SVAO <- flats$rajon_СВАО
flats$SZAO <- flats$rajon_СЗАО
flats$TsAO <- flats$rajon_ЦАО
flats$YuAO <- flats$rajon_ЮАО
flats$YuVAO <- flats$rajon_ЮВАО
flats$YuZAO <- flats$rajon_ЮЗАО

flats <- select(flats, -c(rajon_ВАО, rajon_ЗАО, rajon_ЗелАО, rajon_САО,
                 rajon_СВАО, rajon_СЗАО, rajon_ЦАО,  rajon_ЮАО, rajon_ЮВАО
                 ,rajon_ЮЗАО))


#Тип ремонта
flats$no_repair <- flats$repair_NA+flats$`repair_Без ремонта`
flats$design_repair <- flats$repair_Дизайнерский
flats$euro_repair <- flats$repair_Евроремонт
flats$kosmet_repair <- flats$repair_Косметический

flats <- select(flats, -c(repair_NA, `repair_Без ремонта`,
                          repair_Дизайнерский, repair_Евроремонт,
                          repair_Косметический))


#все переменные в числовые
flats$height <- as.numeric(flats$height)
flats$number_of_rooms <- as.numeric(flats$number_of_rooms)

#тест для подвыборки без NA
flats <- na.omit(flats)
#закончили с обработкой данных


#графики

# плотность цен аренды

flats %>% 
  ggplot(aes(x=price)) +
  geom_density( fill="dodgerblue", alpha=0.5)+
  scale_x_continuous(labels = comma)+
  geom_vline(xintercept=mean(flats$price, na.rm = TRUE),
             linetype="dashed", size=0.5, color="red")+
  geom_vline(xintercept=median(flats$price, na.rm = TRUE),
             linetype="dashed", size=0.5, color="black",show_guide = TRUE)+
  ggtitle("График распределения стоимости аренды квартиры в месяц") +
  +geom_text(data = mean(flats$price, na.rm = TRUE), aes(x = price, label = mean), 
             y = 0.1, angle = 90, vjust = -0.2)
  xlab("Рублей в месяц") + ylab("Плотность") + xlim(0,200000)

#Цветной график разброса в пространстве (площадь;стоимость аренды)
qplot(data=flats,total_area, price, main="График разброса стоимостей в зависимости от площади",
      xlab ="Общая площадь квартиры", ylab="Стоимость аренды в месяц")+geom_point(color="darkblue")+
  geom_smooth(method='lm')

qplot(data=flats, living_area, price, main="График разброса стоимостей в зависимости от площади",
      xlab ="Доля жилой площади квартиры", ylab="Стоимость аренды в месяц")+geom_point(color="darkblue")+geom_smooth(method='lm')


#плотность распределения цен по районам
flats%>%
  ggplot(aes(x=price, fill=rajon)) +
  geom_density(alpha=0.3)+ 
  scale_x_continuous(labels = comma)+xlim(0,100000)+
  labs(x= "Цена аренды (рублей/месяц)",
       subtitle="Плотности распределения цен аренды по районам Москвы")

flats <- select(flats, -rajon)



#простая описательная статистика
summary(select(flats,c(price,number_of_rooms,total_area, living_area, discrim_text,
                       rieltor)),digits=2)

stargazer(as.data.frame(select(flats,c(price,number_of_rooms,total_area, living_area, discrim_text,
                         rieltor))),digits = 1)



# подключим функцию для робастных ошибок HC1
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob) }



cor(flats)
CORR <- cor(flats)
corrplot(CORR, type = "lower", order = "hclust", tl.col = "black", tl.srt = 70)


# Из соображений о том, что переменная Price имеет порядок десятков и сотен тысяч,
#было бы разумно взять логарифм от этой переменной и добавить в датасет
flats$ln_price <- log(flats$price)

#оценим регрессию цены на площадь, дискриминацию и на то, находится ли квартира в центре Москвы
M1 <- lm(data=flats, ln_price~total_area+living_area+discrim_text+TsAO +euro_repair)


#посмотрим на значимости коэффициентов
stargazer(M1)
#на любом разумном уровне значимости все регрессоры важны

flats$metro_dist2 <- flats$metro_dist*flats$on_foot

# базовая линейная модель с включением сразу всех переменных
M2 <- lm(data=flats, ln_price ~ total_area + living_area +
           +discrim_text +
           number_of_rooms + parking + balkon + chute +
           height + lift  +floor + total_floor + metro_dist +
           on_foot + metro_dist2 + rieltor +
           blochniy + kirpichniy +
           monolitniy + monolitno_kirpichniy + 
           stalinskiy +starii_fond +
           VAO + ZAO + ZelAO + SAO + SVAO + SZAO +  YuAO +
           YuVAO +YuZAO + euro_repair + design_repair + 
           kosmet_repair)

#посмотрим на значимости коэффициентов
stargazer(M2)


#Cтроим модель без ключевой переменной и применяем тест Вальда
M3 <- lm(data=flats, ln_price ~ total_area + living_area +
           number_of_rooms + parking + balkon +  chute +
           height + lift  +floor + total_floor + metro_dist +
           on_foot + metro_dist2 + rieltor +
           blochniy +kirpichniy +
           monolitniy + monolitno_kirpichniy + 
           stalinskiy +starii_fond +
           VAO + ZAO + ZelAO + SAO + SVAO + SZAO +  YuAO +
           YuVAO +YuZAO + euro_repair + design_repair + 
           kosmet_repair)

summary(M3)
waldtest(M2,M3,test ="Chisq")
bptest(M3)

stargazer(M1, M2, M3, se=list(cse(M1),cse(M2),cse(M3)),
          title="Модели стоимостей аренды",type ='text', df=FALSE, digits=2)



