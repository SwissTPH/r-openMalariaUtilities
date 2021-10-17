test <- openMalariaUtilities:::.makeXmlRoot(
  schemaVersion = 43L,
  name = "test",
  analysisNo = NULL
)

expected <- xml2::read_xml(
  "<om:scenario xmlns:om=\"http://openmalaria.org/schema/scenario_43\"
xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
name=\"test\"
schemaVersion=\"43\"
xsi:schemaLocation=\"http://openmalaria.org/schema/scenario_43 scenario_43.xsd\"/>"
)

test_that(".makeXmlRoot is working correctly", {
  expect_equal(test, expected)
})
