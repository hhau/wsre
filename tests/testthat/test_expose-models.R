context("Check to see if model compilation worked")

expect_s4_class(
  object = weighted_normal_stanmodel(),
  class = "stanmodel"
)