test_that(".two_digit_month works", {
  CombinedDat <- data.frame(Date = c("2020-05-03", "2020-08-03", "2020-11-03"))
  actual <- .twoDigitMonth(CombinedDat)
  expected <- c("05", "08", "11")
  expect_equal(actual, expected)

  CombinedDat <- data.frame(Date = c("2020-05-03", "2020-15-15", "2020-30-03"))
  expect_error(.twoDigitMonth(CombinedDat))
})

alle <- read.delim(text = "survey	age_group	measure	value	scenario
2	1	0	341	wucase_19
2	2	0	213	wucase_19
2	1	3	101	wucase_19
2	2	3	105	wucase_19
3	1	0	342	wucase_19
3	2	0	213	wucase_19
3	1	3	101	wucase_19
3	2	3	108	wucase_19
4	1	0	340	wucase_19
4	2	0	212	wucase_19
4	1	3	99	wucase_19
4	2	3	105	wucase_19
5	1	0	342	wucase_19
5	2	0	211	wucase_19
5	1	3	100	wucase_19
5	2	3	96	wucase_19
6	1	0	340	wucase_19
6	2	0	211	wucase_19
6	1	3	100	wucase_19
6	2	3	100	wucase_19
7	1	0	341	wucase_19
7	2	0	207	wucase_19
7	1	3	94	wucase_19
7	2	3	99	wucase_19
2	1	0	342	wucase_20
2	2	0	204	wucase_20
2	1	3	102	wucase_20
2	2	3	120	wucase_20
3	1	0	341	wucase_20
3	2	0	206	wucase_20
3	1	3	113	wucase_20
3	2	3	120	wucase_20
4	1	0	342	wucase_20
4	2	0	207	wucase_20
4	1	3	118	wucase_20
4	2	3	112	wucase_20
5	1	0	340	wucase_20
5	2	0	207	wucase_20
5	1	3	123	wucase_20
5	2	3	128	wucase_20
6	1	0	341	wucase_20
6	2	0	205	wucase_20
6	1	3	116	wucase_20
6	2	3	120	wucase_20
7	1	0	340	wucase_20
7	2	0	205	wucase_20
7	1	3	111	wucase_20
7	2	3	106	wucase_20", sep = "\t", header = T)


test_that(".widenProcessedDataset works as expected", {
  match_measure_to_number <- read.delim(text = "V1	V2	V3
0	0	0
1	1	1
2	2	2
3	3	3", sep = "\t")

  rownames(match_measure_to_number) <- c("nHost", "nInfect", "nExpectd", "nPatent")

  actual <- .widenProcessedDataset(data = alle, match_measure_to_number)
  expected <- read.delim(text = "survey	age_group	scenario	nHost_0	nPatent_3	rownum
2	1	wucase_19	341	101	1
2	1	wucase_20	342	102	2
2	2	wucase_19	213	105	3
2	2	wucase_20	204	120	4
3	1	wucase_19	342	101	5
3	1	wucase_20	341	113	6
3	2	wucase_19	213	108	7
3	2	wucase_20	206	120	8
4	1	wucase_19	340	99	9
4	1	wucase_20	342	118	10
4	2	wucase_19	212	105	11
4	2	wucase_20	207	112	12
5	1	wucase_19	342	100	13
5	1	wucase_20	340	123	14
5	2	wucase_19	211	96	15
5	2	wucase_20	207	128	16
6	1	wucase_19	340	100	17
6	1	wucase_20	341	116	18
6	2	wucase_19	211	100	19
6	2	wucase_20	205	120	20
7	1	wucase_19	341	94	21
7	1	wucase_20	340	111	22
7	2	wucase_19	207	99	23
7	2	wucase_20	205	106	24", sep = "\t")

  expect_equal(actual$nHost_0, expected$nHost_0)
  expect_equal(actual$nPatent_3, expected$nPatent_3)
  expect_equal(actual$age_group, expected$age_group)
  expect_equal(actual$survey, expected$survey)
})
