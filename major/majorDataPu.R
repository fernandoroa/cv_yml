persondata1 <- read.table(text = "
field english spanish portuguese
name         \"Fernando Roa\" \"Fernando Roa\"
email        \"froao at unal.edu.co\"
skype        \"ferroao\"
id -
idIssuePlace -
idIssueDate -
profId -
profIdDate -
birthdate -
birthCity -
phonewa -
address -
", fill = T, header = T, stringsAsFactors = F)
persondata1[persondata1 == ""] <- NA
persondata1 <- data.frame(t(apply(persondata1, 1, zoo::na.locf)))
attr(persondata1, "dataType") <- "personItem"
