#' Bivalvia Key
#'
#' This function allows you to identify your bivalve based on their morphology. You should start with an empty
#' folder titled "Bivalvia" where you will eventually store photos of the morphological structures you've used
#' to identify your bivalves. KEEP SEPARATE FROM YOUR OTHER FOLDERS/PICUTRES OF ORGANISMS! This function
#' requires a single input: the path to this folder. It's ok if it's empty to begin with, but in order to run the
#' function you do need to provide it's path on your computer. This path should be contained in quotes and end in
#' a forward slash "/". Once you provide the path, you can begin keying your bivalve! As you move through the
#' identification, you will need to enter numbers based on the interactive questions. Once you populate images
#' into your folder, you will see them plot side by side on the questions they apply to! Important: when naming
#' images, use the following structure "choice_#.jpg" where the pound sign "#" should be replaced by the number
#' you would choose in the key based on the morphological structure you've photographed. You should have no more
#' than one photo per number.
#'
#' @param image.folder Path to the folder that contains your identification images
#' @return A list of your key choices as well as a species Identification!
#' @export

bivalvia <-function(image.folder) {

  image_fdir <- list.files(image.folder)

  a<-"NA"
  b<-"NA"
  c<-"NA"
  d<-"NA"
  e<-"NA"
  f<-"NA"
  g<-"NA"
  h<-"NA"
  i<-"NA"
  j<-"NA"

  a_plot<-"NA"
  b_plot<-"NA"
  c_plot<-"NA"
  d_plot<-"NA"
  e_plot<-"NA"
  f_plot<-"NA"
  g_plot<-"NA"
  h_plot<-"NA"
  i_plot<-"NA"
  j_plot<-"NA"

  order <- "Order not identified"
  family <- "Family not identified"
  genus_species <- "Identification not found"

  if ("choice_2.jpg" %in% image_fdir == TRUE && "choice_4.jpg" %in% image_fdir == TRUE) {
        image_path<- paste0(image.folder, "choice_2.jpg")
        choice2<-magick::image_read(image_path)
        op <- par(mfrow=c(1,2))
        plot(choice2)
        title(main=expression(bold("ENTER 2")), line = -4)

        image_path <- paste0(image.folder, "choice_4.jpg")
        choice4<-magick::image_read(image_path)
        plot(choice4)
        title(main=expression(bold("ENTER 4")), line = -4)
      } else {
        if ("choice_2.jpg" %in% image_fdir == TRUE){
          image_path <- paste0(image.folder, "choice_2.jpg")
          choice2<-magick::image_read(image_path)
          plot(choice2)
          title(main=expression(bold("ENTER 2")))

        }
        if ("choice_4.jpg" %in% image_fdir == TRUE){
          image_path <- paste0(image.folder, "choice_4.jpg")
          choice4<-magick::image_read(image_path)
          plot(choice4)
          title(main=expression(bold("ENTER 4")))

        }

      }

      a <- readline(cat("Which applies to your organism?

      Without lamellar gills; ctenidia resembling those of aspidobranch gastropods (figs. 5 and 6) (ENTER 2)

      With lamellar gills clearly modified for suspension feeding (figs. 7 and 8) (ENTER 4)
"))

      if (is.null(dev.list()) == FALSE){
        dev.off()
      }

  if (a=="2"){
        order<-"Protobranchia"
        print("Your organism is in the order Protobranchia!")

        if ("choice_3.jpg" %in% image_fdir == TRUE && "choice_44.jpg" %in% image_fdir == TRUE) {
          image_path<- paste0(image.folder, "choice_3.jpg")
          choice3<-magick::image_read(image_path)
          op <- par(mfrow=c(1,2))
          plot(choice3)
          title(main=expression(bold("ENTER 3")), line = -4)

          image_path <- paste0(image.folder, "choice_44.jpg")
          choice44<-magick::image_read(image_path)
          plot(choice44)
          title(main=expression(bold("ENTER 44")), line = -4)
        } else {
          if ("choice_3.jpg" %in% image_fdir == TRUE){
            image_path <- paste0(image.folder, "choice_3.jpg")
            choice3<-magick::image_read(image_path)
            plot(choice3)
            title(main=expression(bold("ENTER 3")))

          }
          if ("choice_44.jpg" %in% image_fdir == TRUE){
            image_path <- paste0(image.folder, "choice_44.jpg")
            choice44<-magick::image_read(image_path)
            plot(choice44)
            title(main=expression(bold("ENTER 44")))

          }

        }

        b <- readline(cat("Which applies to your organism?

        Protobranchs feeding by palp proboscides; without lamellar gills;
        with taxodont dentition (fig. 5) (ENTER 3)

        Modified protobranch with reduced palps; hinge of elongate shell without dentition;
        with lustrous yellow-brown periostracum, radially marked and extending beyond shell margins (ENTER 44) Solemya velum"))

        if (is.null(dev.list()) == FALSE){
          dev.off()
        }
      } else {
    if (a=="4") {

          if ("choice_5.jpg" %in% image_fdir == TRUE && "choice_6.jpg" %in% image_fdir == TRUE) {
            image_path<- paste0(image.folder, "choice_5.jpg")
            choice5<-magick::image_read(image_path)
            op <- par(mfrow=c(1,2))
            plot(choice5)
            title(main=expression(bold("ENTER 5")), line = -4)

            image_path <- paste0(image.folder, "choice_6.jpg")
            choice6<-magick::image_read(image_path)
            plot(choice6)
            title(main=expression(bold("ENTER 6")), line = -4)
          } else {
            if ("choice_5.jpg" %in% image_fdir == TRUE){
              image_path <- paste0(image.folder, "choice_5.jpg")
              choice5<-magick::image_read(image_path)
              plot(choice5)
              title(main=expression(bold("ENTER 5")))

            }
            if ("choice_6.jpg" %in% image_fdir == TRUE){
              image_path <- paste0(image.folder, "choice_6.jpg")
              choice6<-magick::image_read(image_path)
              plot(choice6)
              title(main=expression(bold("ENTER 6")))

            }

          }

          b <- readline(cat("Which applies to your organism?

          With lamellar gills, but retaining taxodont dentition (fig. 17) (ENTER 5)

          With lamellar gills, with other than taxodont dentition (ENTER 6)"))

          if (is.null(dev.list()) == FALSE){
            dev.off()
          }

        } else {
      if (a=="NA"){

            } else {
           print("ERROR! Not a choice!")
      }
    }
  }

  if (b=="3") {

    if ("choice_3a.jpg" %in% image_fdir == TRUE && "choice_3b.jpg" %in% image_fdir == TRUE) {
      image_path<- paste0(image.folder, "choice_3a.jpg")
      choice3a<-magick::image_read(image_path)
      op <- par(mfrow=c(1,2))
      plot(choice3a)
      title(main=expression(bold("ENTER 3a")), line = -4)

      image_path <- paste0(image.folder, "choice_3b.jpg")
      choice3b<-magick::image_read(image_path)
      plot(choice3b)
      title(main=expression(bold("ENTER 3b")), line = -4)
    } else {
      if ("choice_3a.jpg" %in% image_fdir == TRUE){
        image_path <- paste0(image.folder, "choice_3a.jpg")
        choice3a<-magick::image_read(image_path)
        plot(choice3a)
        title(main=expression(bold("ENTER 3a")))

      }
      if ("choice_3b.jpg" %in% image_fdir == TRUE){
        image_path <- paste0(image.folder, "choice_3b.jpg")
        choice3b<-magick::image_read(image_path)
        plot(choice3b)
        title(main=expression(bold("ENTER 3b")))

      }

    }

    c <- readline(cat("Which applies to your organism?

    Obliquely ovate globose shell, usually greenish-gray (ENTER 3a)

    Asymmetric, elongate shell, narrowing posteriorly; usually greenish-brown (ENTER 3b)"))

    if (is.null(dev.list()) == FALSE){
      dev.off()
    }

  } else {
    if (b=="44") {
      family<-"Solemyidae"
      genus_species <- "Solemya velum"
      print("Your organism is in the Solemyidae family!")
      print ("Identification! Solemya velum (choice 44)")
    } else {
      if (b=="5") {

        family<-"Arcidae"
        print("Your organism is in the Arcidae family!")

        if ("choice_5a.jpg" %in% image_fdir == TRUE && "choice_5b.jpg" %in% image_fdir == TRUE) {
          image_path<- paste0(image.folder, "choice_5a.jpg")
          choice5a<-magick::image_read(image_path)
          op <- par(mfrow=c(1,2))
          plot(choice5a)
          title(main=expression(bold("ENTER 5a")), line = -4)

          image_path <- paste0(image.folder, "choice_5b.jpg")
          choice5b<-magick::image_read(image_path)
          plot(choice5b)
          title(main=expression(bold("ENTER 5b")), line = -4)
        } else {
          if ("choice_5a.jpg" %in% image_fdir == TRUE){
            image_path <- paste0(image.folder, "choice_5a.jpg")
            choice5a<-magick::image_read(image_path)
            plot(choice5a)
            title(main=expression(bold("ENTER 5a")))

          }
          if ("choice_5b.jpg" %in% image_fdir == TRUE){
            image_path <- paste0(image.folder, "choice_5b.jpg")
            choice5b<-magick::image_read(image_path)
            plot(choice5b)
            title(main=expression(bold("ENTER 5b")))

          }

        }

        c <- readline(cat("Which applies to your organism?

        Rhomboidal shell (up to 3.8 cm); 30-50 ribs; gray-brown periostracum usually worn;
        longer stouter ligament clearly distinct; left valve overlapping right valve (ENTER 5a)

        Ovate shell (up to 5.9 cm); 26-35 ribs; hairy black-brown periostracum persistant;
        narrow ligament less distinct (ENTER 5b)"))

        if (is.null(dev.list()) == FALSE){
          dev.off()
        }

      } else {
        if (b=="6") {

          if ("choice_7.jpg" %in% image_fdir == TRUE && "choice_12.jpg" %in% image_fdir == TRUE && "choice_19.jpg" %in% image_fdir == TRUE) {
            image_path<- paste0(image.folder, "choice_7.jpg")
            choice7<-magick::image_read(image_path)
            op <- par(mfrow=c(2,2))
            plot(choice7)
            title(main=expression( bold("ENTER 7")))

            image_path <- paste0(image.folder, "choice_12.jpg")
            choice12<-magick::image_read(image_path)
            plot(choice12)
            title(main=expression(bold("ENTER 12")))

            image_path <- paste0(image.folder, "choice_19.jpg")
            choice19<-magick::image_read(image_path)
            plot(choice19)
            title(main=expression(bold("ENTER 19")))

          } else {
            if ("choice_7.jpg" %in% image_fdir == TRUE && "choice_12.jpg" %in% image_fdir == TRUE) {
              image_path<- paste0(image.folder, "choice_7.jpg")
              choice7<-magick::image_read(image_path)
              op <- par(mfrow=c(1,2))
              plot(choice7)
              title(main=expression( bold("ENTER 7")), line = -4)

              image_path <- paste0(image.folder, "choice_12.jpg")
              choice12<-magick::image_read(image_path)
              plot(choice12)
              title(main=expression(bold("ENTER 12")), line = -4)
            }

            if ("choice_7.jpg" %in% image_fdir == TRUE && "choice_19.jpg" %in% image_fdir == TRUE) {
              image_path<- paste0(image.folder, "choice_7.jpg")
              choice7<-magick::image_read(image_path)
              op <- par(mfrow=c(1,2))
              plot(choice7)
              title(main=expression( bold("ENTER 7")), line = -4)

              image_path <- paste0(image.folder, "choice_19.jpg")
              choice19<-magick::image_read(image_path)
              plot(choice19)
              title(main=expression(bold("ENTER 19")), line = -4)
            }

            if ("choice_12.jpg" %in% image_fdir == TRUE && "choice_19.jpg" %in% image_fdir == TRUE) {
              image_path<- paste0(image.folder, "choice_12.jpg")
              choice12<-magick::image_read(image_path)
              op <- par(mfrow=c(1,2))
              plot(choice12)
              title(main=expression( bold("ENTER 12")), line = -4)

              image_path <- paste0(image.folder, "choice_19.jpg")
              choice19<-magick::image_read(image_path)
              plot(choice19)
              title(main=expression(bold("ENTER 19")), line = -4)
            } else {
              if ("choice_7.jpg" %in% image_fdir == TRUE){
                image_path <- paste0(image.folder, "choice_7.jpg")
                choice7<-magick::image_read(image_path)
                plot(choice7)
                title(main=expression(bold("ENTER 7")))

              }
              if ("choice_12.jpg" %in% image_fdir == TRUE){
                image_path <- paste0(image.folder, "choice_12.jpg")
                choice12<-magick::image_read(image_path)
                plot(choice12)
                title(main=expression(bold("ENTER 12")))

              }
              if ("choice_19.jpg" %in% image_fdir == TRUE){
                image_path <- paste0(image.folder, "choice_19.jpg")
                choice19<-magick::image_read(image_path)
                plot(choice19)
                title(main=expression(bold("ENTER 19")))

              }
            }
          }

          c<-readline(cat("Which applies to your organism?

          Monomyarian condition of adductor muscle (fig. 18) (ENTER 7)

          Markedly heteromyarian condition of adductor muscles (fig. 16) (ENTER 12)

          Dimyarian condition (or nearly so) of adductor muscles (fig. 17) (ENTER 19)"))


          if (is.null(dev.list()) == FALSE){
            dev.off()
          }
        } else {
          if (b=="NA"){

          } else {
            print("ERROR! Not a choice!")
          }
        }
      }
    }
  }

  if (c=="3a") {
    family<-"Nuculidae"
    genus_species <- "Nucula proxima"
    print("Your organism is in the Nuculidae family!")
    print ("Identification! Nucula proxima (choice 3a)")
  } else {
    if (c=="3b") {
      family<-"Yoldiidae "
      genus_species <- "Yoldia limatula"
      print("Your organism is in the Yoldiidae  family!")
      print ("Identification! Yoldia limatula (choice 3b)")
    } else {
      if (c=="5a") {
        print ("See us! We don't think you will find these, but we could absolutely be wrong! Before you call us over, have your morphological reasoning ready (ie tell us why you chose what you did)")
      } else {
        if (c=="5b") {
          print ("See us! We don't think you will find these, but we could absolutely be wrong! Before you call us over, have your morphological reasoning ready (ie tell us why you chose what you did)")
        } else {
          if (c=="7") {

          if ("choice_10.jpg" %in% image_fdir == TRUE && "choice_8.jpg" %in% image_fdir == TRUE) {
            image_path<- paste0(image.folder, "choice_10.jpg")
            choice10<-magick::image_read(image_path)
            op <- par(mfrow=c(1,2))
            plot(choice10)
            title(main=expression(bold("ENTER 10")), line = -4)

            image_path <- paste0(image.folder, "choice_8.jpg")
            choice8<-magick::image_read(image_path)
            plot(choice8)
            title(main=expression(bold("ENTER 8")), line = -4)
          } else {
            if ("choice_10.jpg" %in% image_fdir == TRUE){
              image_path <- paste0(image.folder, "choice_10.jpg")
              choice10<-magick::image_read(image_path)
              plot(choice10)
              title(main=expression(bold("ENTER 10")))

            }
            if ("choice_8.jpg" %in% image_fdir == TRUE){
              image_path <- paste0(image.folder, "choice_8.jpg")
              choice8<-magick::image_read(image_path)
              plot(choice8)
              title(main=expression(bold("ENTER 8")))

            }

          }

          d <- readline(cat("Which applies to your organism?

          Swimming monomyarian bivalves (scallops) (ENTER 10)

          Attached monomyarian bivalves (oysters and jingle shells) (ENTER 8)"))

          if (is.null(dev.list()) == FALSE){
            dev.off()
          }

        } else {
            if (c=="12") {

            if ("choice_18.jpg" %in% image_fdir == TRUE && "choice_13.jpg" %in% image_fdir == TRUE && "choice_16.jpg" %in% image_fdir == TRUE) {
              image_path<- paste0(image.folder, "choice_18.jpg")
              choice18<-magick::image_read(image_path)
              op <- par(mfrow=c(2,2))
              plot(choice18)
              title(main=expression( bold("ENTER 18")))

              image_path <- paste0(image.folder, "choice_13.jpg")
              choice13<-magick::image_read(image_path)
              plot(choice13)
              title(main=expression(bold("ENTER 13")))

              image_path <- paste0(image.folder, "choice_16.jpg")
              choice16<-magick::image_read(image_path)
              plot(choice16)
              title(main=expression(bold("ENTER 16")))

            } else {
              if ("choice_18.jpg" %in% image_fdir == TRUE && "choice_13.jpg" %in% image_fdir == TRUE) {
                image_path<- paste0(image.folder, "choice_18.jpg")
                choice18<-magick::image_read(image_path)
                op <- par(mfrow=c(1,2))
                plot(choice18)
                title(main=expression( bold("ENTER 18")), line = -4)

                image_path <- paste0(image.folder, "choice_13.jpg")
                choice13<-magick::image_read(image_path)
                plot(choice13)
                title(main=expression(bold("ENTER 13")), line = -4)
              }

              if ("choice_18.jpg" %in% image_fdir == TRUE && "choice_16.jpg" %in% image_fdir == TRUE) {
                image_path<- paste0(image.folder, "choice_18.jpg")
                choice18<-magick::image_read(image_path)
                op <- par(mfrow=c(1,2))
                plot(choice18)
                title(main=expression( bold("ENTER 18")), line = -4)

                image_path <- paste0(image.folder, "choice_16.jpg")
                choice16<-magick::image_read(image_path)
                plot(choice16)
                title(main=expression(bold("ENTER 16")), line = -4)
              }

              if ("choice_13.jpg" %in% image_fdir == TRUE && "choice_16.jpg" %in% image_fdir == TRUE) {
                image_path<- paste0(image.folder, "choice_13.jpg")
                choice13<-magick::image_read(image_path)
                op <- par(mfrow=c(1,2))
                plot(choice13)
                title(main=expression( bold("ENTER 13")), line = -4)

                image_path <- paste0(image.folder, "choice_16.jpg")
                choice16<-magick::image_read(image_path)
                plot(choice16)
                title(main=expression(bold("ENTER 16")), line = -4)
              } else {
                if ("choice_18.jpg" %in% image_fdir == TRUE){
                  image_path <- paste0(image.folder, "choice_18.jpg")
                  choice18<-magick::image_read(image_path)
                  plot(choice18)
                  title(main=expression(bold("ENTER 18")))

                }
                if ("choice_13.jpg" %in% image_fdir == TRUE){
                  image_path <- paste0(image.folder, "choice_13.jpg")
                  choice13<-magick::image_read(image_path)
                  plot(choice13)
                  title(main=expression(bold("ENTER 13")))

                }
                if ("choice_16.jpg" %in% image_fdir == TRUE){
                  image_path <- paste0(image.folder, "choice_16.jpg")
                  choice16<-magick::image_read(image_path)
                  plot(choice16)
                  title(main=expression(bold("ENTER 16")))

                }
              }
            }

            d<-readline(cat("Which applies to your organism?

            Shipworms boring in timber;
            worm-like bodies (fig. 25) bearing reduced valves anteriorly and pallets posteriorly (ENTER 18)

            Wedge shaped shells; markedly narrow anteriorly with well developed, usually dark, periostracum;
            byssal attachment; lacking siphons (mussels) (ENTER 13)

            Markedly elongate shells; active burrowers in sand;
            with well developed foot and short or medium length separate siphons (ENTER 16)"))


            if (is.null(dev.list()) == FALSE){
              dev.off()
            }
          } else {
              if (c=="19") {

              if ("choice_20.jpg" %in% image_fdir == TRUE && "choice_23.jpg" %in% image_fdir == TRUE && "choice_29.jpg" %in% image_fdir == TRUE) {
                image_path<- paste0(image.folder, "choice_20.jpg")
                choice20<-magick::image_read(image_path)
                op <- par(mfrow=c(2,2))
                plot(choice20)
                title(main=expression( bold("ENTER 20")))

                image_path <- paste0(image.folder, "choice_23.jpg")
                choice23<-magick::image_read(image_path)
                plot(choice23)
                title(main=expression(bold("ENTER 23")))

                image_path <- paste0(image.folder, "choice_29.jpg")
                choice29<-magick::image_read(image_path)
                plot(choice29)
                title(main=expression(bold("ENTER 29")))

              } else {
                if ("choice_20.jpg" %in% image_fdir == TRUE && "choice_23.jpg" %in% image_fdir == TRUE) {
                  image_path<- paste0(image.folder, "choice_20.jpg")
                  choice20<-magick::image_read(image_path)
                  op <- par(mfrow=c(1,2))
                  plot(choice20)
                  title(main=expression( bold("ENTER 20")), line = -4)

                  image_path <- paste0(image.folder, "choice_23.jpg")
                  choice23<-magick::image_read(image_path)
                  plot(choice23)
                  title(main=expression(bold("ENTER 23")), line = -4)
                }

                if ("choice_20.jpg" %in% image_fdir == TRUE && "choice_29.jpg" %in% image_fdir == TRUE) {
                  image_path<- paste0(image.folder, "choice_20.jpg")
                  choice20<-magick::image_read(image_path)
                  op <- par(mfrow=c(1,2))
                  plot(choice20)
                  title(main=expression( bold("ENTER 20")), line = -4)

                  image_path <- paste0(image.folder, "choice_29.jpg")
                  choice29<-magick::image_read(image_path)
                  plot(choice29)
                  title(main=expression(bold("ENTER 29")), line = -4)
                }

                if ("choice_23.jpg" %in% image_fdir == TRUE && "choice_29.jpg" %in% image_fdir == TRUE) {
                  image_path<- paste0(image.folder, "choice_23.jpg")
                  choice23<-magick::image_read(image_path)
                  op <- par(mfrow=c(1,2))
                  plot(choice23)
                  title(main=expression( bold("ENTER 23")), line = -4)

                  image_path <- paste0(image.folder, "choice_29.jpg")
                  choice29<-magick::image_read(image_path)
                  plot(choice29)
                  title(main=expression(bold("ENTER 29")), line = -4)
                } else {
                  if ("choice_20.jpg" %in% image_fdir == TRUE){
                    image_path <- paste0(image.folder, "choice_20.jpg")
                    choice20<-magick::image_read(image_path)
                    plot(choice20)
                    title(main=expression(bold("ENTER 20")))

                  }
                  if ("choice_23.jpg" %in% image_fdir == TRUE){
                    image_path <- paste0(image.folder, "choice_23.jpg")
                    choice23<-magick::image_read(image_path)
                    plot(choice23)
                    title(main=expression(bold("ENTER 23")))

                  }
                  if ("choice_29.jpg" %in% image_fdir == TRUE){
                    image_path <- paste0(image.folder, "choice_29.jpg")
                    choice29<-magick::image_read(image_path)
                    plot(choice29)
                    title(main=expression(bold("ENTER 29")))

                  }
                }
              }

              d<-readline(cat("Which applies to your organism?

              Deep burrowing or boring bivalves with massive fused siphons (fig. 21) (ENTER 20)

              Deposit feeding bivalves with separate extensible siphons (fig. 22) (ENTER 23)

              Otherwise, usually with short fringed siphons (fig. 23) (ENTER 29)"))


              if (is.null(dev.list()) == FALSE){
                dev.off()
              }
            } else {
                if (c=="NA"){

              } else {
                  print("ERROR! Not a choice!")
                }
              }
            }
          }
        }
      }
    }
  }

  if (d=="10") {
    print ("See us! We don't think you will find these, but we could absolutely be wrong! Before you call us over, have your morphological reasoning ready (ie tell us why you chose what you did)")
  } else {
    if (d=="8") {

      if ("choice_9.jpg" %in% image_fdir == TRUE && "choice_8a.jpg" %in% image_fdir == TRUE) {
        image_path<- paste0(image.folder, "choice_9.jpg")
        choice9<-magick::image_read(image_path)
        op <- par(mfrow=c(1,2))
        plot(choice9)
        title(main=expression(bold("ENTER 9")), line = -4)

        image_path <- paste0(image.folder, "choice_8a.jpg")
        choice8a<-magick::image_read(image_path)
        plot(choice8a)
        title(main=expression(bold("ENTER 8a")), line = -4)
      } else {
        if ("choice_9.jpg" %in% image_fdir == TRUE){
          image_path <- paste0(image.folder, "choice_9.jpg")
          choice9<-magick::image_read(image_path)
          plot(choice9)
          title(main=expression(bold("ENTER 9")))

        }
        if ("choice_8a.jpg" %in% image_fdir == TRUE){
          image_path <- paste0(image.folder, "choice_8a.jpg")
          choice8a<-magick::image_read(image_path)
          plot(choice8a)
          title(main=expression(bold("ENTER 8a")))

        }

      }

      e <- readline(cat("Which applies to your organism?

      Lower bivalve with hole through which calcified byssus passes (ENTER 9)

      No such hole; lower valve directly cemented to hard substrata (Eastern oyster) (ENTER 8a)"))

      if (is.null(dev.list()) == FALSE){
        dev.off()
      }

    } else {
      if (d=="18") {
        print ("See us! We don't think you will find these, but we could absolutely be wrong! Before you call us over, have your morphological reasoning ready (ie tell us why you chose what you did)")
      } else {
        if (d=="13") {

          if ("choice_14.jpg" %in% image_fdir == TRUE && "choice_13a.jpg" %in% image_fdir == TRUE) {
            image_path<- paste0(image.folder, "choice_14.jpg")
            choice14<-magick::image_read(image_path)
            op <- par(mfrow=c(1,2))
            plot(choice14)
            title(main=expression(bold("ENTER 14")), line = -4)

            image_path <- paste0(image.folder, "choice_13a.jpg")
            choice13a<-magick::image_read(image_path)
            plot(choice13a)
            title(main=expression(bold("ENTER 13a")), line = -4)
          } else {
            if ("choice_14.jpg" %in% image_fdir == TRUE){
              image_path <- paste0(image.folder, "choice_14.jpg")
              choice14<-magick::image_read(image_path)
              plot(choice14)
              title(main=expression(bold("ENTER 14")))

            }
            if ("choice_13a.jpg" %in% image_fdir == TRUE){
              image_path <- paste0(image.folder, "choice_13a.jpg")
              choice13a<-magick::image_read(image_path)
              plot(choice13a)
              title(main=expression(bold("ENTER 13a")))

            }

          }

          e <- readline(cat("Which applies to your organism?

          Umbo near but not at anterior tip of each valve (fig. 16) (ENTER 14)

          Umbo at anterior tip of each valve; no shell sculpture;
          blue-black with shiny periostracum; common in littoral, often abundant (ENTER 13a)"))

          if (is.null(dev.list()) == FALSE){
            dev.off()
          }

        } else {
          if (d=="16") {
            print ("See us! We don't think you will find these, but we could absolutely be wrong! Before you call us over, have your morphological reasoning ready (ie tell us why you chose what you did)")
          } else {
            if (d=="20") {

              if ("choice_21.jpg" %in% image_fdir == TRUE && "choice_20a.jpg" %in% image_fdir == TRUE) {
                image_path<- paste0(image.folder, "choice_21.jpg")
                choice21<-magick::image_read(image_path)
                op <- par(mfrow=c(1,2))
                plot(choice21)
                title(main=expression(bold("ENTER 21")), line = -4)

                image_path <- paste0(image.folder, "choice_20a.jpg")
                choice20a<-magick::image_read(image_path)
                plot(choice20a)
                title(main=expression(bold("ENTER 20a")), line = -4)
              } else {
                if ("choice_21.jpg" %in% image_fdir == TRUE){
                  image_path <- paste0(image.folder, "choice_21.jpg")
                  choice21<-magick::image_read(image_path)
                  plot(choice21)
                  title(main=expression(bold("ENTER 21")))

                }
                if ("choice_20a.jpg" %in% image_fdir == TRUE){
                  image_path <- paste0(image.folder, "choice_20a.jpg")
                  choice20a<-magick::image_read(image_path)
                  plot(choice20a)
                  title(main=expression(bold("ENTER 20a")))

                }

              }

              e <- readline(cat("Which applies to your organism?

              Stout rasping spines externally on anterior of valves; reduced periostracum;
              borers in peat, clay, or stone (ENTER 21)

              Thin valves with irregular growth rings but no sculpture;
              thick periostracum on ventral margin and siphons;
              up to 15 cm shell length; burrowing in muddy sand (ENTER 20a)"))

              if (is.null(dev.list()) == FALSE){
                dev.off()
              }

            }  else {
              if (d=="23") {
                print ("See us! We don't think you will find these, but we could absolutely be wrong! Before you call us over, have your morphological reasoning ready (ie tell us why you chose what you did)")
              } else {
                if (d=="29") {

                  if ("choice_30.jpg" %in% image_fdir == TRUE && "choice_29a.jpg" %in% image_fdir == TRUE && "choice_29b.jpg" %in% image_fdir == TRUE) {
                    image_path<- paste0(image.folder, "choice_30.jpg")
                    choice30<-magick::image_read(image_path)
                    op <- par(mfrow=c(2,2))
                    plot(choice30)
                    title(main=expression( bold("ENTER 30")))

                    image_path <- paste0(image.folder, "choice_29a.jpg")
                    choice29a<-magick::image_read(image_path)
                    plot(choice29a)
                    title(main=expression(bold("ENTER 29a")))

                    image_path <- paste0(image.folder, "choice_29b.jpg")
                    choice29b<-magick::image_read(image_path)
                    plot(choice29b)
                    title(main=expression(bold("ENTER 29b")))

                  } else {
                    if ("choice_30.jpg" %in% image_fdir == TRUE && "choice_29a.jpg" %in% image_fdir == TRUE) {
                      image_path<- paste0(image.folder, "choice_30.jpg")
                      choice30<-magick::image_read(image_path)
                      op <- par(mfrow=c(1,2))
                      plot(choice30)
                      title(main=expression( bold("ENTER 30")), line = -4)

                      image_path <- paste0(image.folder, "choice_29a.jpg")
                      choice29a<-magick::image_read(image_path)
                      plot(choice29a)
                      title(main=expression(bold("ENTER 29a")), line = -4)
                    }

                    if ("choice_30.jpg" %in% image_fdir == TRUE && "choice_29b.jpg" %in% image_fdir == TRUE) {
                      image_path<- paste0(image.folder, "choice_30.jpg")
                      choice30<-magick::image_read(image_path)
                      op <- par(mfrow=c(1,2))
                      plot(choice30)
                      title(main=expression( bold("ENTER 30")), line = -4)

                      image_path <- paste0(image.folder, "choice_29b.jpg")
                      choice29b<-magick::image_read(image_path)
                      plot(choice29b)
                      title(main=expression(bold("ENTER 29b")), line = -4)
                    }

                    if ("choice_29a.jpg" %in% image_fdir == TRUE && "choice_29b.jpg" %in% image_fdir == TRUE) {
                      image_path<- paste0(image.folder, "choice_29a.jpg")
                      choice29a<-magick::image_read(image_path)
                      op <- par(mfrow=c(1,2))
                      plot(choice29a)
                      title(main=expression( bold("ENTER 29a")), line = -4)

                      image_path <- paste0(image.folder, "choice_29b.jpg")
                      choice29b<-magick::image_read(image_path)
                      plot(choice29b)
                      title(main=expression(bold("ENTER 29b")), line = -4)
                    } else {
                      if ("choice_30.jpg" %in% image_fdir == TRUE){
                        image_path <- paste0(image.folder, "choice_30.jpg")
                        choice30<-magick::image_read(image_path)
                        plot(choice30)
                        title(main=expression(bold("ENTER 30")))

                      }
                      if ("choice_29a.jpg" %in% image_fdir == TRUE){
                        image_path <- paste0(image.folder, "choice_29a.jpg")
                        choice29a<-magick::image_read(image_path)
                        plot(choice29a)
                        title(main=expression(bold("ENTER 29a")))

                      }
                      if ("choice_29b.jpg" %in% image_fdir == TRUE){
                        image_path <- paste0(image.folder, "choice_29b.jpg")
                        choice29b<-magick::image_read(image_path)
                        plot(choice29b)
                        title(main=expression(bold("ENTER 29b")))

                      }
                    }
                  }

                  e<-readline(cat("Which applies to your organism?

                  Subglobular shells (ENTER 30)

                  Elongate ovate shell, tapering and becoming compressed posteriorly;
                  periostracum with fine radial lines; often with adherent sand grains;
                  up to 2 cm shell length (ENTER 29a)

                  Entire shell compressed; with umbones near anterior corner of nearly rectangular valves;
                  somewhat saddle shaped; up to 3.4 cm across (ENTER 29b)"))


                  if (is.null(dev.list()) == FALSE){
                    dev.off()
                  }
                } else {
                  if (d=="NA"){

                  } else {
                    print("ERROR! Not a choice!")
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  if (e=="9") {
    family<-"Anomiidae"
    print("Your organism is in the Anomiidae family!")

    if ("choice_9a.jpg" %in% image_fdir == TRUE && "choice_9b.jpg" %in% image_fdir == TRUE) {
      image_path<- paste0(image.folder, "choice_9a.jpg")
      choice9a<-magick::image_read(image_path)
      op <- par(mfrow=c(1,2))
      plot(choice9a)
      title(main=expression(bold("ENTER 9a")), line = -4)

      image_path <- paste0(image.folder, "choice_9b.jpg")
      choice9b<-magick::image_read(image_path)
      plot(choice9b)
      title(main=expression(bold("ENTER 9b")), line = -4)
    } else {
      if ("choice_9a.jpg" %in% image_fdir == TRUE){
        image_path <- paste0(image.folder, "choice_9a.jpg")
        choice9a<-magick::image_read(image_path)
        plot(choice9a)
        title(main=expression(bold("ENTER 9a")))

      }
      if ("choice_9b.jpg" %in% image_fdir == TRUE){
        image_path <- paste0(image.folder, "choice_9b.jpg")
        choice9b<-magick::image_read(image_path)
        plot(choice9b)
        title(main=expression(bold("ENTER 9b")))

      }

    }

    f <- readline(cat("Which applies to your organism?

    Upper valve translucent and smooth; up to 5.1 cm (ENTER 9a)

    Upper valve drably opaque; rough with small prickles (ENTER 9b)"))

    if (is.null(dev.list()) == FALSE){
      dev.off()
    }

  } else {
    if (e=="8a") {
      family<-"Ostreidae"
      genus_species <- "Crassostrea virginica"
      print("Your organism is in the Ostreidae family!")
      print ("Identification! Crassostrea virginica (choice 8a)")
    } else {
      if (e=="14") {

        family<-"Mytilidae"
        print("Your organism is in the Mytilidae family!")

        if ("choice_15.jpg" %in% image_fdir == TRUE && "choice_14a.jpg" %in% image_fdir == TRUE) {
          image_path<- paste0(image.folder, "choice_15.jpg")
          choice15<-magick::image_read(image_path)
          op <- par(mfrow=c(1,2))
          plot(choice15)
          title(main=expression(bold("ENTER 15")), line = -4)

          image_path <- paste0(image.folder, "choice_14a.jpg")
          choice14a<-magick::image_read(image_path)
          plot(choice14a)
          title(main=expression(bold("ENTER 14a")), line = -4)
        } else {
          if ("choice_15.jpg" %in% image_fdir == TRUE){
            image_path <- paste0(image.folder, "choice_15.jpg")
            choice15<-magick::image_read(image_path)
            plot(choice15)
            title(main=expression(bold("ENTER 15")))

          }
          if ("choice_14a.jpg" %in% image_fdir == TRUE){
            image_path <- paste0(image.folder, "choice_14a.jpg")
            choice14a<-magick::image_read(image_path)
            plot(choice14a)
            title(main=expression(bold("ENTER 14a")))

          }

        }

        f <- readline(cat("Which applies to your organism?

        With radial shell sculpture and thin periostracum (ENTER 15)

        Without radial shell sculpture; with thick hairy periostracum; usually dark brown; up to 15 cm;
        less common species; lower littoral and subtidal (ENTER 14a)"))

        if (is.null(dev.list()) == FALSE){
          dev.off()
        }

      } else {
        if (e=="13a") {
          family<-"Mytilidae"
          genus_species <- "Mytilus edulis"
          print("Your organism is in the Mytilidae family!")
          print ("Identification! Mytilus edulis (choice 13a)")
        } else {
          if (e=="21") {
            print ("See us! We don't think you will find these, but we could absolutely be wrong! Before you call us over, have your morphological reasoning ready (ie tell us why you chose what you did)")
          } else {
            if (e=="20a") {
              family<-"Myidae"
              genus_species <- "Mya arenaria"
              print("Your organism is in the Myidae family!")
              print ("Identification! Mya arenaria (choice 20a)")
            } else {
              if (e=="30") {

                if ("choice_31.jpg" %in% image_fdir == TRUE && "choice_30a.jpg" %in% image_fdir == TRUE && "choice_34.jpg" %in% image_fdir == TRUE) {
                  image_path<- paste0(image.folder, "choice_31.jpg")
                  choice31<-magick::image_read(image_path)
                  op <- par(mfrow=c(2,2))
                  plot(choice31)
                  title(main=expression( bold("ENTER 31")))

                  image_path <- paste0(image.folder, "choice_30a.jpg")
                  choice30a<-magick::image_read(image_path)
                  plot(choice30a)
                  title(main=expression(bold("ENTER 30a")))

                  image_path <- paste0(image.folder, "choice_34.jpg")
                  choice34<-magick::image_read(image_path)
                  plot(choice34)
                  title(main=expression(bold("ENTER 34")))

                } else {
                  if ("choice_31.jpg" %in% image_fdir == TRUE && "choice_30a.jpg" %in% image_fdir == TRUE) {
                    image_path<- paste0(image.folder, "choice_31.jpg")
                    choice31<-magick::image_read(image_path)
                    op <- par(mfrow=c(1,2))
                    plot(choice31)
                    title(main=expression( bold("ENTER 31")), line = -4)

                    image_path <- paste0(image.folder, "choice_30a.jpg")
                    choice30a<-magick::image_read(image_path)
                    plot(choice30a)
                    title(main=expression(bold("ENTER 30a")), line = -4)
                  }

                  if ("choice_31.jpg" %in% image_fdir == TRUE && "choice_34.jpg" %in% image_fdir == TRUE) {
                    image_path<- paste0(image.folder, "choice_31.jpg")
                    choice31<-magick::image_read(image_path)
                    op <- par(mfrow=c(1,2))
                    plot(choice31)
                    title(main=expression( bold("ENTER 31")), line = -4)

                    image_path <- paste0(image.folder, "choice_34.jpg")
                    choice34<-magick::image_read(image_path)
                    plot(choice34)
                    title(main=expression(bold("ENTER 34")), line = -4)
                  }

                  if ("choice_30a.jpg" %in% image_fdir == TRUE && "choice_34.jpg" %in% image_fdir == TRUE) {
                    image_path<- paste0(image.folder, "choice_30a.jpg")
                    choice30a<-magick::image_read(image_path)
                    op <- par(mfrow=c(1,2))
                    plot(choice30a)
                    title(main=expression( bold("ENTER 30a")), line = -4)

                    image_path <- paste0(image.folder, "choice_34.jpg")
                    choice34<-magick::image_read(image_path)
                    plot(choice34)
                    title(main=expression(bold("ENTER 34")), line = -4)
                  } else {
                    if ("choice_31.jpg" %in% image_fdir == TRUE){
                      image_path <- paste0(image.folder, "choice_31.jpg")
                      choice31<-magick::image_read(image_path)
                      plot(choice31)
                      title(main=expression(bold("ENTER 31")))

                    }
                    if ("choice_30a.jpg" %in% image_fdir == TRUE){
                      image_path <- paste0(image.folder, "choice_30a.jpg")
                      choice30a<-magick::image_read(image_path)
                      plot(choice30a)
                      title(main=expression(bold("ENTER 30a")))

                    }
                    if ("choice_34.jpg" %in% image_fdir == TRUE){
                      image_path <- paste0(image.folder, "choice_34.jpg")
                      choice34<-magick::image_read(image_path)
                      plot(choice34)
                      title(main=expression(bold("ENTER 34")))

                    }
                  }
                }

                f<-readline(cat("Which applies to your organism?

                Shells with concentric sculpture (ENTER 31)

                Shell with strong radial sculpture (ENTER 30a)

                Shells with no sculpture apart from irregular growth rings (ENTER 34)"))


                if (is.null(dev.list()) == FALSE){
                  dev.off()
                }
              } else {
                if (e=="29a") {
                  print ("See us! We don't think you will find these, but we could absolutely be wrong! Before you call us over, have your morphological reasoning ready (ie tell us why you chose what you did)")
                } else {
                  if (e=="29b") {
                    print ("See us! We don't think you will find these, but we could absolutely be wrong! Before you call us over, have your morphological reasoning ready (ie tell us why you chose what you did)")
                  } else{
                    if (e=="NA"){

                    } else {
                      print("ERROR! Not a choice!")
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  if (f=="9a") {
    genus_species <- "Anomia simplex"
    print ("Identification! Anomia simplex (choice 9a)")
           } else {
    if (f=="9b") {
      genus_species <- " Anomia aculeata"
      print ("Identification!  Anomia aculeata (choice 9b)")
    } else {
      if (f=="15") {

        family<-"Mytilidae"
        print("Your organism is in the Mytilidae family!")

        if ("choice_15a.jpg" %in% image_fdir == TRUE && "choice_15b.jpg" %in% image_fdir == TRUE) {
          image_path<- paste0(image.folder, "choice_15a.jpg")
          choice15a<-magick::image_read(image_path)
          op <- par(mfrow=c(1,2))
          plot(choice15a)
          title(main=expression(bold("ENTER 15a")), line = -4)

          image_path <- paste0(image.folder, "choice_15b.jpg")
          choice15b<-magick::image_read(image_path)
          plot(choice15b)
          title(main=expression(bold("ENTER 15b")), line = -4)
        } else {
          if ("choice_15a.jpg" %in% image_fdir == TRUE){
            image_path <- paste0(image.folder, "choice_15a.jpg")
            choice15a<-magick::image_read(image_path)
            plot(choice15a)
            title(main=expression(bold("ENTER 15a")))

          }
          if ("choice_15b.jpg" %in% image_fdir == TRUE){
            image_path <- paste0(image.folder, "choice_15b.jpg")
            choice15b<-magick::image_read(image_path)
            plot(choice15b)
            title(main=expression(bold("ENTER 15b")))

          }

        }

        g <- readline(cat("Which applies to your organism?

        Hinge without teeth; strong radial ribs which bifurcate, usually covering entire shell;
        up to 10 cm long; salt marshes and upper littoral; often abundant (fig. 16) (ENTER 15a)

        Hinge finely dentate; ribs on anterior and posterior thirds of shell only; up to 6.4 cm long;
        rarer species; subtidal in this area (ENTER 15b)"))

        if (is.null(dev.list()) == FALSE){
          dev.off()
        }

      } else {
        if (f=="14a") {
          genus_species <- "Modiolus modiolus"
          print ("Identification! Modiolus modiolus (choice 14a)")
        } else {
          if (f=="31") {
            if ("choice_32.jpg" %in% image_fdir == TRUE && "choice_31a.jpg" %in% image_fdir == TRUE) {
              image_path<- paste0(image.folder, "choice_32.jpg")
              choice32<-magick::image_read(image_path)
              op <- par(mfrow=c(1,2))
              plot(choice32)
              title(main=expression(bold("ENTER 32")), line = -4)

              image_path <- paste0(image.folder, "choice_31a.jpg")
              choice31a<-magick::image_read(image_path)
              plot(choice31a)
              title(main=expression(bold("ENTER 31a")), line = -4)
            } else {
              if ("choice_32.jpg" %in% image_fdir == TRUE){
                image_path <- paste0(image.folder, "choice_32.jpg")
                choice32<-magick::image_read(image_path)
                plot(choice32)
                title(main=expression(bold("ENTER 32")))

              }
              if ("choice_31a.jpg" %in% image_fdir == TRUE){
                image_path <- paste0(image.folder, "choice_31a.jpg")
                choice31a<-magick::image_read(image_path)
                plot(choice31a)
                title(main=expression(bold("ENTER 31a")))

              }

            }

            g <- readline(cat("Which applies to your organism?

            Ligament external (ENTER 32)

            Ligament internal, triangular; up to 10 mm across (ENTER 31a)"))

            if (is.null(dev.list()) == FALSE){
              dev.off()
            }

          } else {
            if (f=="30a") {
              family<-"Carditidae"
              genus_species <- "Cardita borealis"
              print("Your organism is in the Carditidae family!")
              print ("Identification! Cardita borealis (choice 30a)")
            } else {
              if (f=="34") {

                if ("choice_39.jpg" %in% image_fdir == TRUE && "choice_36.jpg" %in% image_fdir == TRUE && "choice_35.jpg" %in% image_fdir == TRUE) {
                  image_path<- paste0(image.folder, "choice_39.jpg")
                  choice39<-magick::image_read(image_path)
                  op <- par(mfrow=c(2,2))
                  plot(choice39)
                  title(main=expression( bold("ENTER 39")))

                  image_path <- paste0(image.folder, "choice_36.jpg")
                  choice36<-magick::image_read(image_path)
                  plot(choice36)
                  title(main=expression(bold("ENTER 36")))

                  image_path <- paste0(image.folder, "choice_35.jpg")
                  choice35<-magick::image_read(image_path)
                  plot(choice35)
                  title(main=expression(bold("ENTER 35")))

                } else {
                  if ("choice_39.jpg" %in% image_fdir == TRUE && "choice_36.jpg" %in% image_fdir == TRUE) {
                    image_path<- paste0(image.folder, "choice_39.jpg")
                    choice39<-magick::image_read(image_path)
                    op <- par(mfrow=c(1,2))
                    plot(choice39)
                    title(main=expression( bold("ENTER 39")), line = -4)

                    image_path <- paste0(image.folder, "choice_36.jpg")
                    choice36<-magick::image_read(image_path)
                    plot(choice36)
                    title(main=expression(bold("ENTER 36")), line = -4)
                  }

                  if ("choice_39.jpg" %in% image_fdir == TRUE && "choice_35.jpg" %in% image_fdir == TRUE) {
                    image_path<- paste0(image.folder, "choice_39.jpg")
                    choice39<-magick::image_read(image_path)
                    op <- par(mfrow=c(1,2))
                    plot(choice39)
                    title(main=expression( bold("ENTER 39")), line = -4)

                    image_path <- paste0(image.folder, "choice_35.jpg")
                    choice35<-magick::image_read(image_path)
                    plot(choice35)
                    title(main=expression(bold("ENTER 35")), line = -4)
                  }

                  if ("choice_36.jpg" %in% image_fdir == TRUE && "choice_35.jpg" %in% image_fdir == TRUE) {
                    image_path<- paste0(image.folder, "choice_36.jpg")
                    choice36<-magick::image_read(image_path)
                    op <- par(mfrow=c(1,2))
                    plot(choice36)
                    title(main=expression( bold("ENTER 36")), line = -4)

                    image_path <- paste0(image.folder, "choice_35.jpg")
                    choice35<-magick::image_read(image_path)
                    plot(choice35)
                    title(main=expression(bold("ENTER 35")), line = -4)
                  } else {
                    if ("choice_39.jpg" %in% image_fdir == TRUE){
                      image_path <- paste0(image.folder, "choice_39.jpg")
                      choice39<-magick::image_read(image_path)
                      plot(choice39)
                      title(main=expression(bold("ENTER 39")))

                    }
                    if ("choice_36.jpg" %in% image_fdir == TRUE){
                      image_path <- paste0(image.folder, "choice_36.jpg")
                      choice36<-magick::image_read(image_path)
                      plot(choice36)
                      title(main=expression(bold("ENTER 36")))

                    }
                    if ("choice_35.jpg" %in% image_fdir == TRUE){
                      image_path <- paste0(image.folder, "choice_35.jpg")
                      choice35<-magick::image_read(image_path)
                      plot(choice35)
                      title(main=expression(bold("ENTER 35")))

                    }
                  }
                }

                g<-readline(cat("Which applies to your organism?

                Ligament external (ENTER 39)

                Ligament internal, triangular (ENTER 36)

                Ligament in groove but partly external;
                characteristic small shells with two prominent radial folds (fig. 19)
                running posteriorly from anterior umbones (ENTER 35)"))


                if (is.null(dev.list()) == FALSE){
                  dev.off()
                }
              } else {
                if (f=="NA"){

                } else {
                  print("ERROR! Not a choice!")
                }
              }
            }
          }
        }
      }
    }
  }

  if (g=="15a") {
    genus_species <- "Modiolus demissus"
    print ("Identification! Modiolus demissus (choice 15a)")
  } else {
    if (g=="15b") {
      genus_species <- "Musculus (= Modiolaria) niger"
      print ("Identification! Musculus (= Modiolaria) niger (choice 15b)")
    } else {
      if (g=="32") {
        print ("See us! Likely a juvenile!")
      } else {
        if (g=="31a") {
          genus_species <- "Crassinella (= Gouldia) mactracea"
          print ("Identification! Crassinella (= Gouldia) mactracea (choice 31a)")
        } else {
          if (g=="35") {
            print ("See us! We don't think you will find these, but we could absolutely be wrong! Before you call us over, have your morphological reasoning ready (ie tell us why you chose what you did)")
          } else {
            if (g=="36") {
              print ("See us! We don't think you will find these, but we could absolutely be wrong! Before you call us over, have your morphological reasoning ready (ie tell us why you chose what you did)")
            } else {
              if (g=="39") {

                if ("choice_40.jpg" %in% image_fdir == TRUE && "choice_42.jpg" %in% image_fdir == TRUE) {
                  image_path<- paste0(image.folder, "choice_40.jpg")
                  choice40<-magick::image_read(image_path)
                  op <- par(mfrow=c(1,2))
                  plot(choice40)
                  title(main=expression(bold("ENTER 40")), line = -4)

                  image_path <- paste0(image.folder, "choice_42.jpg")
                  choice42<-magick::image_read(image_path)
                  plot(choice42)
                  title(main=expression(bold("ENTER 42")), line = -4)
                } else {
                  if ("choice_40.jpg" %in% image_fdir == TRUE){
                    image_path <- paste0(image.folder, "choice_40.jpg")
                    choice40<-magick::image_read(image_path)
                    plot(choice40)
                    title(main=expression(bold("ENTER 40")))

                  }
                  if ("choice_42.jpg" %in% image_fdir == TRUE){
                    image_path <- paste0(image.folder, "choice_42.jpg")
                    choice42<-magick::image_read(image_path)
                    plot(choice42)
                    title(main=expression(bold("ENTER 42")))

                  }

                }

                h <- readline(cat("Which applies to your organism?

                Shells under 5 mm shell length (ENTER 40)

                Shells over 5 mm shell length (ENTER 42)"))

                if (is.null(dev.list()) == FALSE){
                  dev.off()
                }

              } else {
                if (g=="NA"){

                } else {
                  print("ERROR! Not a choice!")
                }
              }
            }
          }
        }
      }
    }
  }

  if (h=="40") {
    print ("See us! Likely a juvenile!")
  } else {
    if (h=="42") {

      if ("choice_42a.jpg" %in% image_fdir == TRUE && "choice_43.jpg" %in% image_fdir == TRUE) {
        image_path<- paste0(image.folder, "choice_42a.jpg")
        choice42a<-magick::image_read(image_path)
        op <- par(mfrow=c(1,2))
        plot(choice42a)
        title(main=expression(bold("ENTER 42a")), line = -4)

        image_path <- paste0(image.folder, "choice_43.jpg")
        choice43<-magick::image_read(image_path)
        plot(choice43)
        title(main=expression(bold("ENTER 43")), line = -4)
      } else {
        if ("choice_42a.jpg" %in% image_fdir == TRUE){
          image_path <- paste0(image.folder, "choice_42a.jpg")
          choice42a<-magick::image_read(image_path)
          plot(choice42a)
          title(main=expression(bold("ENTER 42a")))

        }
        if ("choice_43.jpg" %in% image_fdir == TRUE){
          image_path <- paste0(image.folder, "choice_43.jpg")
          choice43<-magick::image_read(image_path)
          plot(choice43)
          title(main=expression(bold("ENTER 43")))

        }

      }

      i <- readline(cat("Which applies to your organism?

      Globular smooth cockle with variable brown patterning on white exterior of shell;
      interior always translucent yellow;
      actively moving through sandy substrates using large, very extensible foot;
      up to 2.5 mm diameter (ENTER 42a) Laevicardium mortoni

      Slimmer but still globose clams without above shell coloring; may be up to 15 cm in diameter (ENTER 43)"))

      if (is.null(dev.list()) == FALSE){
        dev.off()
      }

    } else {
      if (h=="NA"){

      } else {
        print("ERROR! Not a choice!")
      }
    }
  }

  if (i=="43") {

    if ("choice_43a.jpg" %in% image_fdir == TRUE && "choice_43b.jpg" %in% image_fdir == TRUE && "choice_43c.jpg" %in% image_fdir == TRUE) {
      image_path<- paste0(image.folder, "choice_43a.jpg")
      choice43a<-magick::image_read(image_path)
      op <- par(mfrow=c(2,2))
      plot(choice43a)
      title(main=expression( bold("ENTER 43a")))

      image_path <- paste0(image.folder, "choice_43b.jpg")
      choice43b<-magick::image_read(image_path)
      plot(choice43b)
      title(main=expression(bold("ENTER 43b")))

      image_path <- paste0(image.folder, "choice_43c.jpg")
      choice43c<-magick::image_read(image_path)
      plot(choice43c)
      title(main=expression(bold("ENTER 43c")))

    } else {
      if ("choice_43a.jpg" %in% image_fdir == TRUE && "choice_43b.jpg" %in% image_fdir == TRUE) {
        image_path<- paste0(image.folder, "choice_43a.jpg")
        choice43a<-magick::image_read(image_path)
        op <- par(mfrow=c(1,2))
        plot(choice43a)
        title(main=expression( bold("ENTER 43a")), line = -4)

        image_path <- paste0(image.folder, "choice_43b.jpg")
        choice43b<-magick::image_read(image_path)
        plot(choice43b)
        title(main=expression(bold("ENTER 43b")), line = -4)
      }

      if ("choice_43a.jpg" %in% image_fdir == TRUE && "choice_43c.jpg" %in% image_fdir == TRUE) {
        image_path<- paste0(image.folder, "choice_43a.jpg")
        choice43a<-magick::image_read(image_path)
        op <- par(mfrow=c(1,2))
        plot(choice43a)
        title(main=expression( bold("ENTER 43a")), line = -4)

        image_path <- paste0(image.folder, "choice_43c.jpg")
        choice43c<-magick::image_read(image_path)
        plot(choice43c)
        title(main=expression(bold("ENTER 43c")), line = -4)
      }

      if ("choice_43b.jpg" %in% image_fdir == TRUE && "choice_43c.jpg" %in% image_fdir == TRUE) {
        image_path<- paste0(image.folder, "choice_43b.jpg")
        choice43b<-magick::image_read(image_path)
        op <- par(mfrow=c(1,2))
        plot(choice43b)
        title(main=expression( bold("ENTER 43b")), line = -4)

        image_path <- paste0(image.folder, "choice_43c.jpg")
        choice43c<-magick::image_read(image_path)
        plot(choice43c)
        title(main=expression(bold("ENTER 43c")), line = -4)
      } else {
        if ("choice_43a.jpg" %in% image_fdir == TRUE){
          image_path <- paste0(image.folder, "choice_43a.jpg")
          choice43a<-magick::image_read(image_path)
          plot(choice43a)
          title(main=expression(bold("ENTER 43a")))

        }
        if ("choice_43b.jpg" %in% image_fdir == TRUE){
          image_path <- paste0(image.folder, "choice_43b.jpg")
          choice43b<-magick::image_read(image_path)
          plot(choice43b)
          title(main=expression(bold("ENTER 43b")))

        }
        if ("choice_43c.jpg" %in% image_fdir == TRUE){
          image_path <- paste0(image.folder, "choice_43c.jpg")
          choice43c<-magick::image_read(image_path)
          plot(choice43c)
          title(main=expression(bold("ENTER 43c")))

        }
      }
    }

    j<-readline(cat("Which applies to your organism?

    Shell valves with deeply incised lunules anterior to umbones (fig. 28);
    shell may retain traces of juvenile concentric sculpture;
    internal free margin of shells crenulated and usually purple;
    up to 15 cm diameter — Quahog (ENTER 43a) Mercenaria (= Venus) mercenaria

    Shell valves without such lunules and with shiny dark brown
    or black periostracum showing many fine incised growth lines;
    no crenulation on valve margin; up to 13 cm diameter (ENTER 43b) Arctica (= Cyprina) islandica

    Shell valves with large but shallow lunules; dull chalky white;
    without crenulation on ventral margin of valves; up to 5 cm diameter (fig. 29) (ENTER 43c) Pitar morrhuana
"))


    if (is.null(dev.list()) == FALSE){
      dev.off()
    }
  } else {
    if (i=="NA"){

    } else {
      print("ERROR! Not a choice!")
    }
  }


 if (b == "NA") {
        choices<-paste(cat("Your Choice:", a))
      } else {
        if (c == "NA"){
          choices<-paste(cat("Your Choices:", a, b))
        } else {
          if (d == "NA"){
            choices<-paste(cat("Your Choices:", a, b, c))
          } else {
            if (e == "NA"){
              choices<-paste(cat("Your Choices:", a, b, c, d))
            } else {
              if (f == "NA"){
                choices<-paste(cat("Your Choices:", a, b, c, d, e))
              } else {
                if (g == "NA"){
                  choices<-paste(cat("Your Choices:", a, b, c, d, e, f))
                } else {
                  if (h == "NA"){
                    choices<-paste(cat("Your Choices:", a, b, c, d, e, f, g))
                  } else {
                    if (i == "NA"){
                      choices<-paste(cat("Your Choices:", a, b, c, d, e, f, g, h))
                    } else {
                      if (j=="NA"){
                        choices<-paste(cat("Your Choices:", a, b, c, d, e, f, g, h, i))
                      } else{
                        choices<-paste(cat("Your Choices:", a, b, c, d, e, f, g, h, i, j))
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      id<-paste(cat("
Your organism identification is
              Order:", order,
                    "
              Family:", family,
                    "
              Genus/Species:", genus_species,
                    "
"))

      return("No Errors? Congratulations! You are clearly in your dichotomous key era! Slay!! If you have errors, no worries, it can still be a brat summer! Just rerun the function to begin again!")

}
