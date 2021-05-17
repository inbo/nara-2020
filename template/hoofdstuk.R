library(git2rdata)
write_vc(
  data.frame(
    hoofdstuk = c(1, 3, 4, 5),
    volledig = c(
      "Biodiversiteit: het fundament van ons ecosysteem",
      "Algemene toestand en trends in Vlaanderen", "De biodiversiteit onder druk",
      "Trends per ecosysteem"
    ),
    kort = c("fundament", "toestand-trend", "onder-druk", "ecosysteem")
  ),
  file = "template/hoofdstuk", sorting = "hoofdstuk", optimize = FALSE
)
