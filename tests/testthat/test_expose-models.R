context("Check to see if model compilation worked")

expect_s4_class(
  object = normal_stanmodel(),
  class = "stanmodel"
)