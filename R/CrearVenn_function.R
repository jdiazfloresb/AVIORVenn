#' Crea Venn Diagrams
#'
#' Esta funcion permite crear Diagramas de Venn de 2 a 5 variables.
#' @param df datafrane de dos columnas, y con N renglones, donde N es el total de combinaciones menos uno.
#' @export

creaVenn <- function(df = 0) {

  areas <- df
  numRen <- nrow(areas)

  if (numRen == 3) {
    numSets = 2

      # Traemos las variables de la Matriz:
      #     1 = A, 2 = B,
      #     3 = AB

      cross.area <- areas[3, 2]
      area1 <- areas[1, 2] + cross.area
      area2 <- areas[2, 2] + cross.area

      nomA <- areas[1, 1]
      nomB <- areas[2, 1]

      VennDiagram::draw.pairwise.venn(area1,
                         area2,
                         cross.area,
                         category = c(nomA, nomB),
                         fill = c("green", "blue"),
                         lty = "dashed",
                         cex = 2,
                         cat.cex = 2,
                         alpha = .35,
                         cat.col = c("green", "blue"))

      return(sprintf("Se hizo un diagrama de Venn para %s variables", numSets))
  }

  else if (numRen == 7) {
    numSets = 3

      # Traemos las variables de la Matriz:
      A <- areas[1, 2]
      B <- areas[2, 2]
      C <- areas[3, 2]
      AB <- areas[4, 2]
      AC <- areas[5, 2]
      BC <- areas[6, 2]
      ABC <- areas[7, 2]

      nomA <- areas[1, 1]
      nomB <- areas[2, 1]
      nomC <- areas[3, 1]

      #Ponemos las variables en el orden necesario del Diagrama, en un vector
      #     A = 1,  AB = 2, B = 3, AC = 4, ABC = 5, BC = 6, C = 7
      datos3V <- c(A, AB, B, AC, ABC, BC, C)

      VennDiagram::draw.triple.venn(direct.area = TRUE,
                       area.vector = datos3V,
                       category = c(nomA, nomB, nomC),
                       fill = c("green", "blue", "red"),
                       lty = "dashed",
                       cex = 2,
                       cat.cex = 2,
                       alpha = .35,
                       cat.col = c("green", "blue", "red"))

      return(sprintf("Se hizo un diagrama de Venn para %s variables", numSets))
  }

  else if (numRen == 15) {
    numSets = 4

      # Traemos las variables de la Matriz:
      A <- areas[1, 2]
      B <- areas[2, 2]
      C <- areas[3, 2]
      D <- areas[4, 2]
      AB <- areas[5, 2]
      AC <- areas[6, 2]
      AD <- areas[7, 2]
      BC <- areas[8, 2]
      BD <- areas[9, 2]
      CD <- areas[10, 2]
      ABC <- areas[11, 2]
      ABD <- areas[12, 2]
      ACD <- areas[13, 2]
      BCD <- areas[14, 2]
      ABCD <- areas[15, 2]

      nomA <- areas[1, 1]
      nomB <- areas[2, 1]
      nomC <- areas[3, 1]
      nomD <- areas[4, 1]

      #Ponemos las variables en el orden necesario del Diagrama, en un vector
      #     C = 1,  CD = 2, D = 3,  AC = 4, ACD = 5,  ABCD = 6, BCD = 7,  BD = 8,
      #     A = 9,  AD = 10, ABD = 11, ABC = 12, BC = 13, B = 14, AB = 15

      datos4V <- c(C, CD, D, AC, ACD, ABCD, BCD, BD, A, AD, ABD, ABC, BC, B, AB)

      VennDiagram::draw.quad.venn(direct.area = TRUE,
                     area.vector = datos4V,
                     category = c(nomA, nomB, nomC, nomD),
                     fill = c("green", "blue", "red", "orange"),
                     lty = "dashed",
                     cex = 2,
                     cat.cex = 2,
                     alpha = .35,
                     cat.col = c("green", "blue", "red", "orange"))

      return(sprintf("Se hizo un diagrama de Venn para %s variables", numSets))
  }

  else if (numRen == 31) {
    numSets = 5


      # Traemos las variables de la Matriz:
      A <- areas[1, 2]
      B <- areas[2, 2]
      C <- areas[3, 2]
      D <- areas[4, 2]
      E <- areas[5, 2]
      AB <- areas[6, 2]
      AC <- areas[7, 2]
      AD <- areas[8, 2]
      AE <- areas[9, 2]
      BC <- areas[10, 2]
      BD <- areas[11, 2]
      BE <- areas[12, 2]
      CD <- areas[13, 2]
      CE <- areas[14, 2]
      DE <- areas[15, 2]
      ABC <- areas[16, 2]
      ABD <- areas[17, 2]
      ABE <- areas[18, 2]
      ACD <- areas[19, 2]
      ACE <- areas[20, 2]
      ADE <- areas[21, 2]
      BCD <- areas[22, 2]
      BCE <- areas[23, 2]
      BDE <- areas[24, 2]
      CDE <- areas[25, 2]
      ABCD <- areas[26, 2]
      ABCE <- areas[27, 2]
      ABDE<- areas[28, 2]
      ACDE <- areas[29, 2]
      BCDE <- areas[30, 2]
      ABCDE <- areas[31, 2]

      nomA <- areas[1, 1]
      nomB <- areas[2, 1]
      nomC <- areas[3, 1]
      nomD <- areas[4, 1]
      nomE <- areas[5, 1]

      #Ponemos las variables en el orden necesario del Diagrama, en un vector
      #   A = 1,  B = 2,  C = 3,  D = 4,  E = 5
      #   CE = 6, AE = 7, AD = 8, AB = 9, BE = 10, BC = 11, AC = 12, CD = 13, BD = 14, DE = 15
      #   CDE = 16, ACE = 17, ADE = 18, ABD = 19, ABE = 20, BCE = 21, ABC = 22, ACD = 23,
      #   BCD = 24, BDE = 25, BCDE = 26, ACDE = 27, ABDE = 28, ABCE = 29, ABCD = 30, ABCDE = 31

      datos5V <- c(A, B, C, D, E,
                   CE, AE, AD, AB, BE, BC, AC, CD, BD, DE,
                   CDE, ACE, ADE, ABD, ABE, BCE, ABC, ACD,
                   BCE, BDE, BCDE, ACDE, ABDE, ABCE, ABCD, ABCDE)

      VennDiagram::draw.quintuple.venn(direct.area = TRUE,
                          area.vector = datos5V,
                          category = c(nomA, nomB, nomC, nomD, nomE),
                          fill = c("green", "blue", "red", "orange", "gray"),
                          lty = "dashed",
                          cex = 2,
                          cat.cex = 2,
                          alpha = .35,
                          cat.col = c("green", "blue", "red", "orange", "gray"))

      return(sprintf("Se hizo un diagrama de Venn para %s variables", numSets))
  }

  else {
    stop(message("Error en los datos. Pueden faltar combinaciones. Renglones: ", numRen))
  }

  return(numSets)
}
