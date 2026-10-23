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

test_that("supported schema versions have matching namespaces and locations", {
  for (version in c(44L, 49L)) {
    root <- .makeXmlRoot(version, name = "test", analysisNo = NULL)
    namespace <- paste0("http://openmalaria.org/schema/scenario_", version)
    expect_identical(unname(xml2::xml_ns(root)["om"]), namespace)
    expect_identical(xml2::xml_attr(root, "schemaVersion"), as.character(version))
    expect_identical(
      xml2::xml_attr(root, "xsi:schemaLocation", ns = xml2::xml_ns(root)),
      paste(namespace, paste0("scenario_", version, ".xsd"))
    )
  }
})
