## ----init, include=FALSE--------------------------------------------------------------------------
options(width=100)

## ----read-----------------------------------------------------------------------------------------
require(R.HDSS)
file = system.file("extdata/testHDSS.csv", package="R.HDSS")
rawdata = readRawHDSS(file)

## ----show-----------------------------------------------------------------------------------------
str(rawdata)

## ----test1----------------------------------------------------------------------------------------
coreVariableTests(rawdata)

## ----preproc--------------------------------------------------------------------------------------
testdata = preprocHDSS(rawdata)
str(testdata)

## ----test2----------------------------------------------------------------------------------------
coreRecordTests(testdata)

## ----show_recordTests-----------------------------------------------------------------------------
rt = coreRecordTests(testdata)
str(rt)

## ----show_test9-----------------------------------------------------------------------------------
require(dplyr)
filter(testdata, !rt$Index[,9]) %>% select(RecNr:EventDate)

## ----showTest_bg----------------------------------------------------------------------------------
## Start of study
filter(testdata, EventCode %in% c("ENU", "IMG", "BTH")) %>% summarise(min(EventDate))
## All records for the individuals
filter(testdata, IndividualId %in% c(87, 3076)) %>% select(IndividualId:ObservationDate)

## ----delete---------------------------------------------------------------------------------------
testdata2 = filter(testdata, !(IndividualId %in% c(87, 3076)) )

## ----freq_events----------------------------------------------------------------------------------
table(testdata2$EventCode)

## ----freq_first_last------------------------------------------------------------------------------
table(firstEvents(testdata2))
table(lastEvents(testdata2))

## ----trans1---------------------------------------------------------------------------------------
trans = getTransitions(testdata2)

## ----trans_counts---------------------------------------------------------------------------------
trans$counts

## ----trans_deltaT---------------------------------------------------------------------------------
trans$deltaT

## ----trans_avetime--------------------------------------------------------------------------------
with(trans, deltaT["BTH", "DTH"]/counts["BTH", "DTH"])

