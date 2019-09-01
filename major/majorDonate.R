# donatedata1<-read.table(text="
# field english spanish portuguese
# url [![](https://liberapay.com/assets/widgets/donate.svg)](https://liberapay.com/ferroao/donate)
# badges [![](http://img.shields.io/liberapay/receives/ferroao.svg?logo=liberapay)](https://liberapay.com/ferroao/donate)
# ",fill=T,header=T, stringsAsFactors=F)
# donatedata1[donatedata1==""]<-NA
# donatedata1<-data.frame(t(apply(donatedata1, 1, zoo::na.locf)))
# attr(donatedata1, 'dataType') <- "donateItem"
