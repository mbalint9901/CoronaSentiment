mygrid <- data.frame(
  row = c(5, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6),
  col = c(7, 1, 3, 4, 7, 7, 5, 4, 2, 3, 7, 2, 3, 5, 4, 4, 7, 6, 2, 5, 3, 6, 4, 5, 2, 4, 7, 1, 6),
  code = c("BG", "IS", "NO", "FI", "EE", "LV", "SE", "DK", "UK", "NL", "LT", "BE", "DE", "PL", "CZ", "AT", "RO", "HU", "FR", "SK", "CH", "HR", "IT", "SI", "ES", "MT", "CY", "PT", "EL"),
  name = c("Bulgária", "Izland", "Norvégia", "Finnország", "Észtország", "Lettország", "Svédország", "Dánia", "Egyesült Királyság", "Hollandia", "Litvánia", "Belgium", "Németország", "Lengyelország", "Csehország", "Ausztria", "Románia", "Magyarország", "Franciaország", "Szlovákia", "Svájc", "Horvátország", "Olaszország", "Szlovénia", "Spanyolország", "Málta", "Ciprus", "Portugália", "Görögország"),
  stringsAsFactors = FALSE
)
geofacet::grid_preview(mygrid)

