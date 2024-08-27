#' Gastropoda Key
#'
#' This function allows you to identify your gastropod based on their morphology. You should start with an empty
#' folder titled "Gastropoda" where you will eventually store photos of the morphological structures you've used
#' to identify your gastropods. KEEP SEPARATE FROM YOUR OTHER FOLDERS/PICUTRES OF ORGANISMS! This function
#' requires a single input: the path to this folder. It's ok if it's empty to begin with, but in order to run the
#' function you do need to provide it's path on your computer. This path should be contained in quotes and end in
#' a forward slash "/". Once you provide the path, you can begin keying your gastropod! As you move through the
#' identification, you will need to enter numbers based on the interactive questions. Once you populate images
#' into your folder, you will see them plot side by side on the questions they apply to! Important: when naming
#' images, use the following structure "choice_#.jpg" where the pound sign "#" should be replaced by the number
#' you would choose in the key based on the morphological structure you've photographed. You should have no more
#' than one photo per number.
#'
#' @param image.folder Path to the folder that contains your identification images
#' @return A list of your key choices as well as a species Identification!
#' @export

gastropoda<-function(image.folder){

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

  infraorder <- "Infraorder not identified"
  family <- "Family not identified"
  genus_species <- "Identification not found"

  if ("choice_47.jpg" %in% image_fdir == TRUE && "choice_3.jpg" %in% image_fdir == TRUE && "choice_2.jpg" %in% image_fdir == TRUE) {
    image_path<- paste0(image.folder, "choice_47.jpg")
    choice47<-magick::image_read(image_path)
    op <- par(mfrow=c(2,2))
    plot(choice47)
    title(main=expression( bold("ENTER 47")))

    image_path <- paste0(image.folder, "choice_3.jpg")
    choice3<-magick::image_read(image_path)
    plot(choice3)
    title(main=expression(bold("ENTER 3")))

    image_path <- paste0(image.folder, "choice_2.jpg")
    choice2<-magick::image_read(image_path)
    plot(choice2)
    title(main=expression(bold("ENTER 2")))

  } else {
      if ("choice_47.jpg" %in% image_fdir == TRUE && "choice_3.jpg" %in% image_fdir == TRUE) {
      image_path<- paste0(image.folder, "choice_47.jpg")
      choice47<-magick::image_read(image_path)
      op <- par(mfrow=c(1,2))
      plot(choice47)
      title(main=expression( bold("ENTER 47")), line = -4)

      image_path <- paste0(image.folder, "choice_3.jpg")
      choice3<-magick::image_read(image_path)
      plot(choice3)
      title(main=expression(bold("ENTER 3")), line = -4)
    }
      if ("choice_47.jpg" %in% image_fdir == TRUE && "choice_2.jpg" %in% image_fdir == TRUE) {
      image_path<- paste0(image.folder, "choice_47.jpg")
      choice47<-magick::image_read(image_path)
      op <- par(mfrow=c(1,2))
      plot(choice47)
      title(main=expression( bold("ENTER 47")), line = -4)

      image_path <- paste0(image.folder, "choice_2.jpg")
      choice2<-magick::image_read(image_path)
      plot(choice2)
      title(main=expression(bold("ENTER 2")), line = -4)
    }
      if ("choice_3.jpg" %in% image_fdir == TRUE && "choice_2.jpg" %in% image_fdir == TRUE) {
      image_path<- paste0(image.folder, "choice_3.jpg")
      choice3<-magick::image_read(image_path)
      op <- par(mfrow=c(1,2))
      plot(choice3)
      title(main=expression( bold("ENTER 3")), line = -4)

      image_path <- paste0(image.folder, "choice_2.jpg")
      choice2<-magick::image_read(image_path)
      plot(choice2)
      title(main=expression(bold("ENTER 2")), line = -4)
    } else {
        if ("choice_47.jpg" %in% image_fdir == TRUE){
        image_path <- paste0(image.folder, "choice_47.jpg")
        choice47<-magick::image_read(image_path)
        plot(choice47)
        title(main=expression(bold("ENTER 47")))

      }
        if ("choice_3.jpg" %in% image_fdir == TRUE){
        image_path <- paste0(image.folder, "choice_3.jpg")
        choice3<-magick::image_read(image_path)
        plot(choice3)
        title(main=expression(bold("ENTER 3")))

      }
        if ("choice_2.jpg" %in% image_fdir == TRUE){
        image_path <- paste0(image.folder, "choice_2.jpg")
        choice2<-magick::image_read(image_path)
        plot(choice2)
        title(main=expression(bold("ENTER 2")))

      }
    }
  }

  a <- readline(cat("Which applies to your organism?

  Single aspidobranch ctenidium (figs. 1 and 2) and other 'archaeogastropod'
  characters; simple conical shell without internal shelf or recurved apex;
  'tortoise-shell' coloring
  (ENTER 47)

  Single pectinibranch ctenidium (figs. 3 and 4); shells of various forms — coiled, conical, or
  reduced (ENTER 3)

  No ctenidium; mantle cavity as a lung (and other pulmonate characters);
  shiny ovoid shells with no operculum; small salt marsh or high littoral snails
  (ENTER 2)"))

  if (is.null(dev.list()) == FALSE){
    dev.off()
  }

  if (a=="47"){
    family<-"Acmaeidae"
    genus_species <- "Acmaea testudinalis"
    print("Your organism is in the Acmaeidae family!")
    print ("Identification! Acmaea testudinalis (choice 47)")
  } else {
    if (a=="3") {

      if ("choice_31.jpg" %in% image_fdir == TRUE && "choice_4.jpg" %in% image_fdir == TRUE) {
        image_path<- paste0(image.folder, "choice_31.jpg")
        choice31<-magick::image_read(image_path)
        op <- par(mfrow=c(1,2))
        plot(choice31)
        title(main=expression(bold("ENTER 31")), line = -4)

        image_path <- paste0(image.folder, "choice_4.jpg")
        choice4<-magick::image_read(image_path)
        plot(choice4)
        title(main=expression(bold("ENTER 4")), line = -4)
      } else {
        if ("choice_31.jpg" %in% image_fdir == TRUE){
          image_path <- paste0(image.folder, "choice_31.jpg")
          choice31<-magick::image_read(image_path)
          plot(choice31)
          title(main=expression(bold("ENTER 31")))

        }
        if ("choice_4.jpg" %in% image_fdir == TRUE){
          image_path <- paste0(image.folder, "choice_4.jpg")
          choice4<-magick::image_read(image_path)
          plot(choice4)
          title(main=expression(bold("ENTER 4")))

        }

      }

      b <- readline(cat("Which applies to your organism?

      Snails with an inhalent pallial siphon and an extensible proboscis bearing a narrow radula;
      mostly active predaceous carnivores or carrion feeders;
      turbinate coiled shells with a canal or rudimentary notch next to the columella to accommodate the pallial
      siphon (Neogastropoda — whelks and drills) (ENTER 31)

      Snails other than above (ENTER 4)"))

      if (is.null(dev.list()) == FALSE){
        dev.off()
      }

    } else {
      if (a=="2"){
        family<-"Ellobiidae"
        print("Your organism is in the Ellobiidae family!")

        if ("choice_48.jpg" %in% image_fdir == TRUE && "choice_49.jpg" %in% image_fdir == TRUE) {
          image_path<- paste0(image.folder, "choice_48.jpg")
          choice48<-magick::image_read(image_path)
          op <- par(mfrow=c(1,2))
          plot(choice48)
          title(main=expression(bold("ENTER 48")), line = -4)

          image_path <- paste0(image.folder, "choice_49.jpg")
          choice49<-magick::image_read(image_path)
          plot(choice49)
          title(main=expression(bold("ENTER 49")), line = -4)
        } else {
          if ("choice_48.jpg" %in% image_fdir == TRUE){
            image_path <- paste0(image.folder, "choice_48.jpg")
            choice48<-magick::image_read(image_path)
            plot(choice48)
            title(main=expression(bold("ENTER 48")))

          }
          if ("choice_49.jpg" %in% image_fdir == TRUE){
            image_path <- paste0(image.folder, "choice_49.jpg")
            choice49<-magick::image_read(image_path)
            plot(choice49)
            title(main=expression(bold("ENTER 49")))

          }

        }

        b <- readline(cat("Which applies to your organism?

        Squat, egg shaped, widest behind aperture, with very short conical spire;
        translucent brown; adult is about 15 mm long with 5-6 whorls;
        abundant in salt marshes (ENTER 48)

        Top shaped, widest at aperture, with longer conical spire one-third length of shell;
        shiny brownish-yellow;
        adult is about 7.5 mm long with 7-8 whorls and an incised suture;
        less common than above (ENTER 49)"))
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
  }

  if (b=="31") {

    if ("choice_16a.jpg" %in% image_fdir == TRUE && "choice_32.jpg" %in% image_fdir == TRUE) {
      image_path<- paste0(image.folder, "choice_16a.jpg")
      choice16a<-magick::image_read(image_path)
      op <- par(mfrow=c(1,2))
      plot(choice16a)
      title(main=expression(bold("ENTER 16a")), line = -4)

      image_path <- paste0(image.folder, "choice_32.jpg")
      choice32<-magick::image_read(image_path)
      plot(choice32)
      title(main=expression(bold("ENTER 32")), line = -4)

    } else {
      if ("choice_16a.jpg" %in% image_fdir == TRUE){
        image_path <- paste0(image.folder, "choice_16a.jpg")
        choice16a<-magick::image_read(image_path)
        plot(choice16a)
        title(main=expression(bold("ENTER 16a")))

      }

      if ("choice_32.jpg" %in% image_fdir == TRUE){
        image_path <- paste0(image.folder, "choice_32.jpg")
        choice32<-magick::image_read(image_path)
        plot(choice32)
        title(main=expression(bold("ENTER 32")))

    }

  }

  c<-readline(cat("Which applies to your organism?

  Snails with protrusible proboscis and inhalent siphon, but with unsculptured,
  globose shells lacking any siphonal canal or notch (almost certainly naticid moon snail) (ENTER 16a)

  Snails with protrusible proboscis and inhalent siphon, but with turreted shells;
  usually with sculpture and with a siphonal canal or notch near the columella (ENTER 32)"))


  if (is.null(dev.list()) == FALSE){
    dev.off()
    }
  } else {
    if (b=="4") {

      if ("choice_7.jpg" %in% image_fdir == TRUE && "choice_5.jpg" %in% image_fdir == TRUE && "choice_9.jpg" %in% image_fdir == TRUE) {
        image_path<- paste0(image.folder, "choice_7.jpg")
        choice7<-magick::image_read(image_path)
        op <- par(mfrow=c(2,2))
        plot(choice7)
        title(main=expression( bold("ENTER 7")), line = -4)

        image_path <- paste0(image.folder, "choice_5.jpg")
        choice5<-magick::image_read(image_path)
        plot(choice5)
        title(main=expression(bold("ENTER 5")), line = -4)

        image_path <- paste0(image.folder, "choice_9.jpg")
        choice9<-magick::image_read(image_path)
        plot(choice9)
        title(main=expression(bold("ENTER 9")), line = -4)

      } else {
        if ("choice_7.jpg" %in% image_fdir == TRUE && "choice_5.jpg" %in% image_fdir == TRUE) {
          image_path<- paste0(image.folder, "choice_7.jpg")
          choice7<-magick::image_read(image_path)
          op <- par(mfrow=c(1,2))
          plot(choice7)
          title(main=expression( bold("ENTER 7")), line = -4)

          image_path <- paste0(image.folder, "choice_5.jpg")
          choice5<-magick::image_read(image_path)
          plot(choice5)
          title(main=expression(bold("ENTER 5")), line = -4)
        }

        if ("choice_7.jpg" %in% image_fdir == TRUE && "choice_9.jpg" %in% image_fdir == TRUE) {
          image_path<- paste0(image.folder, "choice_7.jpg")
          choice7<-magick::image_read(image_path)
          op <- par(mfrow=c(1,2))
          plot(choice7)
          title(main=expression( bold("ENTER 7")), line = -4)

          image_path <- paste0(image.folder, "choice_9.jpg")
          choice9<-magick::image_read(image_path)
          plot(choice9)
          title(main=expression(bold("ENTER 9")), line = -4)
        }

        if ("choice_5.jpg" %in% image_fdir == TRUE && "choice_9.jpg" %in% image_fdir == TRUE) {
          image_path<- paste0(image.folder, "choice_5.jpg")
          choice5<-magick::image_read(image_path)
          op <- par(mfrow=c(1,2))
          plot(choice5)
          title(main=expression( bold("ENTER 5")), line = -4)

          image_path <- paste0(image.folder, "choice_9.jpg")
          choice9<-magick::image_read(image_path)
          plot(choice9)
          title(main=expression(bold("ENTER 9")), line = -4)
        } else {
          if ("choice_7.jpg" %in% image_fdir == TRUE){
            image_path <- paste0(image.folder, "choice_7.jpg")
            choice7<-magick::image_read(image_path)
            plot(choice7)
            title(main=expression(bold("ENTER 7")))

          }
          if ("choice_5.jpg" %in% image_fdir == TRUE){
            image_path <- paste0(image.folder, "choice_5.jpg")
            choice5<-magick::image_read(image_path)
            plot(choice5)
            title(main=expression(bold("ENTER 5")))

          }
          if ("choice_9.jpg" %in% image_fdir == TRUE){
            image_path <- paste0(image.folder, "choice_9.jpg")
            choice9<-magick::image_read(image_path)
            plot(choice9)
            title(main=expression(bold("ENTER 9")))

          }
        }
      }

      c<-readline(cat("Which applies to your organism?

      Shells with only apex as a regular turbinate spiral shell, continuing to grow as
      cylindrical tube which may be irregularly worm-like or minute cucumber shaped (ENTER 7)

      Cap-shaped, conical shells with internal shelf (ENTER 5)

      Snails other than above (ENTER 9)"))


      if (is.null(dev.list()) == FALSE){
        dev.off()
      }
    } else {
      if (b=="48") {
        genus_species <- "Melampus bidentatus"
        print ("Identification! Melampus bidentatus (choice 48)")
      } else {
        if (b=="49") {
          genus_species <- "Ovatella (= Alexia) myosotis"
          print ("Identification! Ovatella (= Alexia) myosotis (choice 49)")
        } else {
          if (b=="NA"){

          } else {
            print("ERROR! Not a choice!")
          }
        }
      }
    }
  }

  if (c=="16a") {

      if ("choice_17.jpg" %in% image_fdir == TRUE){
        image_path <- paste0(image.folder, "choice_17.jpg")
        choice17<-magick::image_read(image_path)
        plot(choice17)
        title(main=expression(bold("ENTER 17")))

      }

    d<-readline(cat("Which applies to your organism?

    Globose shells (shaped like helicid land snails), lacking any siphonal canal on
    shell, with brown horny operculum characteristically D-shaped; in living animal
    enormous expanded foot partially encloses shell as animal plows along on sandy
    substrate; proboscis extrusible but not usually visible; short retractible fleshy
    inhalent pallial siphon; predaceous carnivores (naticid moon snail) (ENTER 17)

    Other than above, recheck alternative characters at couplet 3 and couplet 9
    (ENTER 1; you'll need to restart!)"))


    if (is.null(dev.list()) == FALSE){
      dev.off()
    }
  } else {
    if (c=="32") {

      if ("choice_33.jpg" %in% image_fdir == TRUE && "choice_34.jpg" %in% image_fdir == TRUE) {
        image_path<- paste0(image.folder, "choice_33.jpg")
        choice33<-magick::image_read(image_path)
        op <- par(mfrow=c(1,2))
        plot(choice33)
        title(main=expression(bold("ENTER 33")), line = -4)

        image_path <- paste0(image.folder, "choice_34.jpg")
        choice34<-magick::image_read(image_path)
        plot(choice34)
        title(main=expression(bold("ENTER 34")), line = -4)

      } else {
        if ("choice_33.jpg" %in% image_fdir == TRUE){
          image_path <- paste0(image.folder, "choice_33.jpg")
          choice33<-magick::image_read(image_path)
          plot(choice33)
          title(main=expression(bold("ENTER 33")))

        }

        if ("choice_34.jpg" %in% image_fdir == TRUE){
          image_path <- paste0(image.folder, "choice_34.jpg")
          choice34<-magick::image_read(image_path)
          plot(choice34)
          title(main=expression(bold("ENTER 34")))

        }

      }

      d<-readline(cat("Which applies to your organism?

      Minute, turreted snails less than 5 mm shell length as adult (ENTER 33)

      Snails usually larger than 12 mm shell length when adult (most species much larger) (ENTER 34)"))


      if (is.null(dev.list()) == FALSE){
        dev.off()
      }
    } else {
      if (c=="7") {

        if ("choice_54.jpg" %in% image_fdir == TRUE && "choice_8.jpg" %in% image_fdir == TRUE) {
          image_path<- paste0(image.folder, "choice_54.jpg")
          choice54<-magick::image_read(image_path)
          op <- par(mfrow=c(1,2))
          plot(choice54)
          title(main=expression(bold("ENTER 54")), line = -4)

          image_path <- paste0(image.folder, "choice_8.jpg")
          choice8<-magick::image_read(image_path)
          plot(choice8)
          title(main=expression(bold("ENTER 8")), line = -4)

        } else {
          if ("choice_54.jpg" %in% image_fdir == TRUE){
            image_path <- paste0(image.folder, "choice_54.jpg")
            choice54<-magick::image_read(image_path)
            plot(choice54)
            title(main=expression(bold("ENTER 54")))

          }

          if ("choice_8.jpg" %in% image_fdir == TRUE){
            image_path <- paste0(image.folder, "choice_8.jpg")
            choice8<-magick::image_read(image_path)
            plot(choice8)
            title(main=expression(bold("ENTER 8")))

          }

        }

        d<-readline(cat("Which applies to your organism?

          Attached tubular shell drawn out distally and irregularly twisted to
          resemble a large serpulid worm tube; relatively large, up to 7.6 cm long;
          only one species recorded near Woods Hole (ENTER 54)

          Minute cucumber-shaped molluscs with only a tiny spiral apex (that may be eroded) (ENTER 8)"))


        if (is.null(dev.list()) == FALSE){
          dev.off()
        }
      } else {
        if (c=="5") {
          if ("choice_50.jpg" %in% image_fdir == TRUE && "choice_6.jpg" %in% image_fdir == TRUE) {
            image_path<- paste0(image.folder, "choice_50.jpg")
            choice50<-magick::image_read(image_path)
            op <- par(mfrow=c(1,2))
            plot(choice50)
            title(main=expression(bold("ENTER 50")), line = -4)

            image_path <- paste0(image.folder, "choice_6.jpg")
            choice6<-magick::image_read(image_path)
            plot(choice6)
            title(main=expression(bold("ENTER 6")), line = -4)

          } else {
            if ("choice_50.jpg" %in% image_fdir == TRUE){
              image_path <- paste0(image.folder, "choice_50.jpg")
              choice50<-magick::image_read(image_path)
              plot(choice50)
              title(main=expression(bold("ENTER 50")))

            }

            if ("choice_6.jpg" %in% image_fdir == TRUE){
              image_path <- paste0(image.folder, "choice_6.jpg")
              choice6<-magick::image_read(image_path)
              plot(choice6)
              title(main=expression(bold("ENTER 6")))

            }

          }

          d<-readline(cat("Which applies to your organism?

          Almost circular 'limpet' shell with central cup-shaped shelf internally (ENTER 50)

          Ovate slipper limpet shells with posterior shelf (ENTER 6)"))


          if (is.null(dev.list()) == FALSE){
            dev.off()
          }
        } else {
          if (c=="9") {

            if ("choice_10.jpg" %in% image_fdir == TRUE && "choice_16b.jpg" %in% image_fdir == TRUE && "choice_22.jpg" %in% image_fdir == TRUE) {
              image_path<- paste0(image.folder, "choice_10.jpg")
              choice10<-magick::image_read(image_path)
              op <- par(mfrow=c(2,2))
              plot(choice10)
              title(main=expression( bold("ENTER 10")))

              image_path <- paste0(image.folder, "choice_16b.jpg")
              choice16b<-magick::image_read(image_path)
              plot(choice16b)
              title(main=expression(bold("ENTER 16b")))

              image_path <- paste0(image.folder, "choice_22.jpg")
              choice22<-magick::image_read(image_path)
              plot(choice22)
              title(main=expression(bold("ENTER 22")))

            } else {
              if ("choice_10.jpg" %in% image_fdir == TRUE && "choice_16b.jpg" %in% image_fdir == TRUE) {
                image_path<- paste0(image.folder, "choice_10.jpg")
                choice10<-magick::image_read(image_path)
                op <- par(mfrow=c(1,2))
                plot(choice10)
                title(main=expression( bold("ENTER 10")), line = -4)

                image_path <- paste0(image.folder, "choice_16b.jpg")
                choice16b<-magick::image_read(image_path)
                plot(choice16b)
                title(main=expression(bold("ENTER 16b")), line = -4)
              }

              if ("choice_10.jpg" %in% image_fdir == TRUE && "choice_22.jpg" %in% image_fdir == TRUE) {
                image_path<- paste0(image.folder, "choice_10.jpg")
                choice10<-magick::image_read(image_path)
                op <- par(mfrow=c(1,2))
                plot(choice10)
                title(main=expression( bold("ENTER 10")), line = -4)

                image_path <- paste0(image.folder, "choice_22.jpg")
                choice22<-magick::image_read(image_path)
                plot(choice22)
                title(main=expression(bold("ENTER 22")), line = -4)
              }

              if ("choice_16b.jpg" %in% image_fdir == TRUE && "choice_22.jpg" %in% image_fdir == TRUE) {
                image_path<- paste0(image.folder, "choice_16b.jpg")
                choice16b<-magick::image_read(image_path)
                op <- par(mfrow=c(1,2))
                plot(choice16b)
                title(main=expression( bold("ENTER 16b")), line = -4)

                image_path <- paste0(image.folder, "choice_22.jpg")
                choice22<-magick::image_read(image_path)
                plot(choice22)
                title(main=expression(bold("ENTER 22")), line = -4)
              } else {
                if ("choice_10.jpg" %in% image_fdir == TRUE){
                  image_path <- paste0(image.folder, "choice_10.jpg")
                  choice10<-magick::image_read(image_path)
                  plot(choice10)
                  title(main=expression(bold("ENTER 10")))

                }
                if ("choice_16b.jpg" %in% image_fdir == TRUE){
                  image_path <- paste0(image.folder, "choice_16b.jpg")
                  choice16b<-magick::image_read(image_path)
                  plot(choice16b)
                  title(main=expression(bold("ENTER 16b")))

                }
                if ("choice_22.jpg" %in% image_fdir == TRUE){
                  image_path <- paste0(image.folder, "choice_22.jpg")
                  choice22<-magick::image_read(image_path)
                  plot(choice22)
                  title(main=expression(bold("ENTER 22")))

                }
              }
            }

            d<-readline(cat("Which applies to your organism?

            Snails with no operculum, and reduced shell that is usually enclosed within
            expanded pallial or pedal tissues in life;
            animal usually incapable of withdrawing completely within shell (ENTER 10)

            Snails with normal operculum and shell,
            though expanded soft parts appear disproportionately large and may conceal shell almost completely;
            but animal is capable of total withdrawal into shell (ENTER 16b)

            Snails with normal operculum and shell; soft parts are readily contained within shell (ENTER 22)"))


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

  if (d=="17") {

    if ("choice_18.jpg" %in% image_fdir == TRUE && "choice_20.jpg" %in% image_fdir == TRUE) {
      image_path<- paste0(image.folder, "choice_18.jpg")
      choice18<-magick::image_read(image_path)
      op <- par(mfrow=c(1,2))
      plot(choice18)
      title(main=expression(bold("ENTER 18")), line = -4)

      image_path <- paste0(image.folder, "choice_20.jpg")
      choice20<-magick::image_read(image_path)
      plot(choice20)
      title(main=expression(bold("ENTER 20")), line = -4)

    } else {
      if ("choice_18.jpg" %in% image_fdir == TRUE){
        image_path <- paste0(image.folder, "choice_18.jpg")
        choice18<-magick::image_read(image_path)
        plot(choice18)
        title(main=expression(bold("ENTER 18")))

      }

      if ("choice_20.jpg" %in% image_fdir == TRUE){
        image_path <- paste0(image.folder, "choice_20.jpg")
        choice20<-magick::image_read(image_path)
        plot(choice20)
        title(main=expression(bold("ENTER 20")))

      }

    }

    e<-readline(cat("Which applies to your organism?

    Shell umbilicus clearly open (fig. 79) (ENTER 18)

    Shell umbilicus totally or nearly occluded by callus (fig. 11) (ENTER 20)"))


    if (is.null(dev.list()) == FALSE){
      dev.off()
    }
  } else {
    if (d=="33") {

      if ("choice_82.jpg" %in% image_fdir == TRUE && "choice_83.jpg" %in% image_fdir == TRUE) {
        image_path<- paste0(image.folder, "choice_82.jpg")
        choice82<-magick::image_read(image_path)
        op <- par(mfrow=c(1,2))
        plot(choice82)
        title(main=expression(bold("ENTER 82")), line = -4)

        image_path <- paste0(image.folder, "choice_83.jpg")
        choice83<-magick::image_read(image_path)
        plot(choice83)
        title(main=expression(bold("ENTER 83")), line = -4)

      } else {
        if ("choice_82.jpg" %in% image_fdir == TRUE){
          image_path <- paste0(image.folder, "choice_82.jpg")
          choice82<-magick::image_read(image_path)
          plot(choice82)
          title(main=expression(bold("ENTER 82")))

        }

        if ("choice_83.jpg" %in% image_fdir == TRUE){
          image_path <- paste0(image.folder, "choice_83.jpg")
          choice83<-magick::image_read(image_path)
          plot(choice83)
          title(main=expression(bold("ENTER 83")))

        }

      }

      e<-readline(cat("Which applies to your organism?

      Smooth glossy shell; gray or yellow-brown with darkened marbling;
      narrow oval aperture; with siphonal notch;
      up to 5 mm shell length, common free-living species on eel grass and elsewhere in littoral (ENTER 82)

      White glossy shells with considerable ornamentation, usually no obvious siphonal notch;
      up to 5 mm shell length; usually ectoparasites (often on specific invertebrates) (ENTER 83)"))


      if (is.null(dev.list()) == FALSE){
        dev.off()
      }
    } else {
      if (d=="34") {

        if ("choice_35.jpg" %in% image_fdir == TRUE && "choice_39.jpg" %in% image_fdir == TRUE) {
          image_path<- paste0(image.folder, "choice_35.jpg")
          choice35<-magick::image_read(image_path)
          op <- par(mfrow=c(1,2))
          plot(choice35)
          title(main=expression(bold("ENTER 35")), line = -4)

          image_path <- paste0(image.folder, "choice_39.jpg")
          choice39<-magick::image_read(image_path)
          plot(choice39)
          title(main=expression(bold("ENTER 39")), line = -4)

        } else {
          if ("choice_35.jpg" %in% image_fdir == TRUE){
            image_path <- paste0(image.folder, "choice_35.jpg")
            choice35<-magick::image_read(image_path)
            plot(choice35)
            title(main=expression(bold("ENTER 35")))

          }

          if ("choice_39.jpg" %in% image_fdir == TRUE){
            image_path <- paste0(image.folder, "choice_39.jpg")
            choice39<-magick::image_read(image_path)
            plot(choice39)
            title(main=expression(bold("ENTER 39")))

          }

        }

        e<-readline(cat("Which applies to your organism?

        Siphonal canal short, forming conspicuous notch in apertural lip (fig. 13) (ENTER 35)

        Siphonal canal elongate, forming an obvious extension of the apertural lip (fig. 12) (ENTER 39)"))


        if (is.null(dev.list()) == FALSE){
          dev.off()
        }
      } else {
        if (d=="54") {
          family<-"Turritellidae"
          genus_species <- "Vermicularia spirata"
          print("Your organism is in the Turritellidae family!")
          print ("Identification! Vermicularia spirata (choice 54)")
        } else {
          if (d=="8") {

            family<-"Caecidae"
            print("Your organism is in the Caecidae family!")

            if ("choice_55.jpg" %in% image_fdir == TRUE && "choice_56.jpg" %in% image_fdir == TRUE) {
              image_path<- paste0(image.folder, "choice_55.jpg")
              choice55<-magick::image_read(image_path)
              op <- par(mfrow=c(1,2))
              plot(choice55)
              title(main=expression(bold("ENTER 55")), line = -4)

              image_path <- paste0(image.folder, "choice_56.jpg")
              choice56<-magick::image_read(image_path)
              plot(choice56)
              title(main=expression(bold("ENTER 56")), line = -4)

            } else {
              if ("choice_55.jpg" %in% image_fdir == TRUE){
                image_path <- paste0(image.folder, "choice_55.jpg")
                choice55<-magick::image_read(image_path)
                plot(choice55)
                title(main=expression(bold("ENTER 55")))

              }

              if ("choice_56.jpg" %in% image_fdir == TRUE){
                image_path <- paste0(image.folder, "choice_56.jpg")
                choice56<-magick::image_read(image_path)
                plot(choice56)
                title(main=expression(bold("ENTER 56")))

              }

            }

            e<-readline(cat("Which applies to your organism?

            Glossy opaque white; with about 15 longitudinal ribs; about 5 mm in length (ENTER 55)

            Translucent tan when alive, chalky white when dead;
            with about 20-30 circular ribs; about 2 mm in length (ENTER 56)"))


            if (is.null(dev.list()) == FALSE){
              dev.off()
            }
          } else {
            if (d=="50") {
              family<-"Calyptraeidae"
              genus_species <- "Crucibulum striatum"
              print("Your organism is in the Calyptraeidae family!")
              print ("Identification! Crucibulum striatum (choice 50)")
            } else {
              if (d=="6") {
                family<-"Calyptraeidae"
                print("Your organism is in the Calyptraeidae family!")

                if ("choice_51.jpg" %in% image_fdir == TRUE && "choice_52.jpg" %in% image_fdir == TRUE && "choice_53.jpg" %in% image_fdir == TRUE) {
                  image_path<- paste0(image.folder, "choice_51.jpg")
                  choice51<-magick::image_read(image_path)
                  op <- par(mfrow=c(2,2))
                  plot(choice51)
                  title(main=expression( bold("ENTER 51")), line = -4)

                  image_path <- paste0(image.folder, "choice_52.jpg")
                  choice52<-magick::image_read(image_path)
                  plot(choice52)
                  title(main=expression(bold("ENTER 52")), line = -4)

                  image_path <- paste0(image.folder, "choice_53.jpg")
                  choice53<-magick::image_read(image_path)
                  plot(choice53)
                  title(main=expression(bold("ENTER 53")), line = -4)

                } else {
                  if ("choice_51.jpg" %in% image_fdir == TRUE && "choice_52.jpg" %in% image_fdir == TRUE) {
                    image_path<- paste0(image.folder, "choice_51.jpg")
                    choice51<-magick::image_read(image_path)
                    op <- par(mfrow=c(1,2))
                    plot(choice51)
                    title(main=expression( bold("ENTER 51")), line = -4)

                    image_path <- paste0(image.folder, "choice_52.jpg")
                    choice52<-magick::image_read(image_path)
                    plot(choice52)
                    title(main=expression(bold("ENTER 52")), line = -4)
                  }

                  if ("choice_51.jpg" %in% image_fdir == TRUE && "choice_53.jpg" %in% image_fdir == TRUE) {
                    image_path<- paste0(image.folder, "choice_51.jpg")
                    choice51<-magick::image_read(image_path)
                    op <- par(mfrow=c(1,2))
                    plot(choice51)
                    title(main=expression( bold("ENTER 51")), line = -4)

                    image_path <- paste0(image.folder, "choice_53.jpg")
                    choice53<-magick::image_read(image_path)
                    plot(choice53)
                    title(main=expression(bold("ENTER 53")), line = -4)
                  }

                  if ("choice_52.jpg" %in% image_fdir == TRUE && "choice_53.jpg" %in% image_fdir == TRUE) {
                    image_path<- paste0(image.folder, "choice_52.jpg")
                    choice52<-magick::image_read(image_path)
                    op <- par(mfrow=c(1,2))
                    plot(choice52)
                    title(main=expression( bold("ENTER 52")), line = -4)

                    image_path <- paste0(image.folder, "choice_53.jpg")
                    choice53<-magick::image_read(image_path)
                    plot(choice53)
                    title(main=expression(bold("ENTER 53")), line = -4)
                  } else {
                    if ("choice_51.jpg" %in% image_fdir == TRUE){
                      image_path <- paste0(image.folder, "choice_51.jpg")
                      choice51<-magick::image_read(image_path)
                      plot(choice51)
                      title(main=expression(bold("ENTER 51")))

                    }
                    if ("choice_52.jpg" %in% image_fdir == TRUE){
                      image_path <- paste0(image.folder, "choice_52.jpg")
                      choice52<-magick::image_read(image_path)
                      plot(choice52)
                      title(main=expression(bold("ENTER 52")))

                    }
                    if ("choice_53.jpg" %in% image_fdir == TRUE){
                      image_path <- paste0(image.folder, "choice_53.jpg")
                      choice53<-magick::image_read(image_path)
                      plot(choice53)
                      title(main=expression(bold("ENTER 53")))

                    }
                  }
                }

                e<-readline(cat("Which applies to your organism?

                Robust slipper limpet; shell opaque dirty white to tan, often with brownish blotches;
                shell height about one third of length; concave white shelf occupies about half of aperture;
                often found in 'stacks'; up to 5 cm long (ENTER 51)

                Very flat slipper limpet; always pearly white;
                variably flexed white shelf occludes less than half of aperture;
                never forms stacks; often inside other shells or on Limulus; up to 3.3 cm long (ENTER 52)

                Small, relatively high slipper limpet;
                apex obvious and often overhanging posterior margin of shell;
                oblique brown shelf occluding about one third of aperture; up to 13 mm long (ENTER 53)"))


                if (is.null(dev.list()) == FALSE){
                  dev.off()
                }
              } else {
                if (d=="10") {

                  if ("choice_11.jpg" %in% image_fdir == TRUE && "choice_12.jpg" %in% image_fdir == TRUE) {
                    image_path<- paste0(image.folder, "choice_11.jpg")
                    choice11<-magick::image_read(image_path)
                    op <- par(mfrow=c(1,2))
                    plot(choice11)
                    title(main=expression(bold("ENTER 11")), line = -4)

                    image_path <- paste0(image.folder, "choice_12.jpg")
                    choice12<-magick::image_read(image_path)
                    plot(choice12)
                    title(main=expression(bold("ENTER 12")), line = -4)

                  } else {
                    if ("choice_11.jpg" %in% image_fdir == TRUE){
                      image_path <- paste0(image.folder, "choice_11.jpg")
                      choice11<-magick::image_read(image_path)
                      plot(choice11)
                      title(main=expression(bold("ENTER 11")))

                    }

                    if ("choice_12.jpg" %in% image_fdir == TRUE){
                      image_path <- paste0(image.folder, "choice_12.jpg")
                      choice12<-magick::image_read(image_path)
                      plot(choice12)
                      title(main=expression(bold("ENTER 12")))

                    }

                  }

                  e<-readline(cat("Which applies to your organism?

                  Shell totally enclosed within animal (ENTER 11)

                  Shell may be partially or largely exposed upon retraction of animal (ENTER 12)

                  (Note: Pteropods and other pelagic gastropods with reduced shells are omitted from this key.)"))


                  if (is.null(dev.list()) == FALSE){
                    dev.off()
                  }
                } else {
                  if (d=="16b") {

                    if ("choice_17.jpg" %in% image_fdir == TRUE){
                      image_path <- paste0(image.folder, "choice_17.jpg")
                      choice17<-magick::image_read(image_path)
                      plot(choice17)
                      title(main=expression(bold("ENTER 17")))

                    }

                    e<-readline(cat("Which applies to your organism?

    Globose shells (shaped like helicid land snails), lacking any siphonal canal on
    shell, with brown horny operculum characteristically D-shaped; in living animal
    enormous expanded foot partially encloses shell as animal plows along on sandy
    substrate; proboscis extrusible but not usually visible; short retractible fleshy
    inhalent pallial siphon; predaceous carnivores (naticid moon snail) (ENTER 17)

    Other than above, recheck alternative characters at couplet 3 and couplet 9
    (ENTER 1; you'll need to restart!)"))


                    if (is.null(dev.list()) == FALSE){
                      dev.off()
                    }
                  } else {
                    if (d=="22") {

                      if ("choice_23.jpg" %in% image_fdir == TRUE && "choice_29.jpg" %in% image_fdir == TRUE) {
                        image_path<- paste0(image.folder, "choice_23.jpg")
                        choice23<-magick::image_read(image_path)
                        op <- par(mfrow=c(1,2))
                        plot(choice23)
                        title(main=expression(bold("ENTER 23")), line = -4)

                        image_path <- paste0(image.folder, "choice_29.jpg")
                        choice29<-magick::image_read(image_path)
                        plot(choice29)
                        title(main=expression(bold("ENTER 29")), line = -4)

                      } else {
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

                      e<-readline(cat("Which applies to your organism?

                      Elongate turret shells (fig. 15); usually more than five obvious whorls;
                      height more than 1.5 times diameter (ENTER 23)

                      Globose shells (fig. 14); height less than 1.5 times diameter (ENTER 29)"))


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
    }
  }

  if (e=="18") {

    if ("choice_19.jpg" %in% image_fdir == TRUE && "choice_64.jpg" %in% image_fdir == TRUE) {
      image_path<- paste0(image.folder, "choice_19.jpg")
      choice19<-magick::image_read(image_path)
      op <- par(mfrow=c(1,2))
      plot(choice19)
      title(main=expression(bold("ENTER 19")), line = -4)

      image_path <- paste0(image.folder, "choice_64.jpg")
      choice64<-magick::image_read(image_path)
      plot(choice64)
      title(main=expression(bold("ENTER 64")), line = -4)

    } else {
      if ("choice_19.jpg" %in% image_fdir == TRUE){
        image_path <- paste0(image.folder, "choice_19.jpg")
        choice19<-magick::image_read(image_path)
        plot(choice19)
        title(main=expression(bold("ENTER 19")))

      }

      if ("choice_64.jpg" %in% image_fdir == TRUE){
        image_path <- paste0(image.folder, "choice_64.jpg")
        choice64<-magick::image_read(image_path)
        plot(choice64)
        title(main=expression(bold("ENTER 64")))

      }

    }

    f<-readline(cat("Which applies to your organism?

    With no obvious callus on the shell (ENTER 19)

    With obvious ivory-white thickened callus that encroaches on the umbilicus;
    shell white with yellowish periostracum; up to l cm long (ENTER 64)"))


    if (is.null(dev.list()) == FALSE){
      dev.off()
    }
  } else {
    if (e=="20") {

      if ("choice_67.jpg" %in% image_fdir == TRUE && "choice_21.jpg" %in% image_fdir == TRUE) {
        image_path<- paste0(image.folder, "choice_67.jpg")
        choice67<-magick::image_read(image_path)
        op <- par(mfrow=c(1,2))
        plot(choice67)
        title(main=expression(bold("ENTER 67")), line = -4)

        image_path <- paste0(image.folder, "choice_21.jpg")
        choice21<-magick::image_read(image_path)
        plot(choice21)
        title(main=expression(bold("ENTER 21")), line = -4)

      } else {
        if ("choice_67.jpg" %in% image_fdir == TRUE){
          image_path <- paste0(image.folder, "choice_67.jpg")
          choice67<-magick::image_read(image_path)
          plot(choice67)
          title(main=expression(bold("ENTER 67")))

        }

        if ("choice_21.jpg" %in% image_fdir == TRUE){
          image_path <- paste0(image.folder, "choice_21.jpg")
          choice21<-magick::image_read(image_path)
          plot(choice21)
          title(main=expression(bold("ENTER 21")))

        }

      }

      f<-readline(cat("Which applies to your organism?

      Shell considerably wider than high, with flat spire; clean bluish-gray,
      with obvious purple (or more rarely brown or pink) callus almost completely occluding umbilicus;
      up to 7.5 cm diameter; often abundant intertidally and in shallow water (ENTER 67)

      Shells slightly higher than wide with obvious spire; white with pale brown periostracum;
      white callus closing or nearly closing umbilicus;
      opercula more calcified than other naticids above;
      smaller species, less common and usually subtidal (ENTER 21)"))


      if (is.null(dev.list()) == FALSE){
        dev.off()
      }
    } else {
      if (e=="82") {
        family<-"Columbellidae"
        genus_species <- "Mitrella lunata"
        print("Your organism is in the Columbellidae family!")
        print ("Identification! Mitrella lunata (choice 82)")
      } else {
        if (e=="83") {
          family<-"PYRAMIDELLIDACEA"
          genus_species <- "depends!"
          print("Your organism is in the PYRAMIDELLIDACEA family!")
          print ("Identification! PYRAMIDELLIDACEA (choice 83) See note, p. 151. Two species have been positively identified in this region: Odostomia (Menestho) bisuturalis — parasitic on Littorina littorea AND Odostomia (Chrysallida) seminuda — parasitic on Crepidula fornicata")
        } else {
          if (e=="35") {

            if ("choice_84.jpg" %in% image_fdir == TRUE && "choice_36.jpg" %in% image_fdir == TRUE) {
              image_path<- paste0(image.folder, "choice_84.jpg")
              choice84<-magick::image_read(image_path)
              op <- par(mfrow=c(1,2))
              plot(choice84)
              title(main=expression(bold("ENTER 84")), line = -4)

              image_path <- paste0(image.folder, "choice_36.jpg")
              choice36<-magick::image_read(image_path)
              plot(choice36)
              title(main=expression(bold("ENTER 36")), line = -4)

            } else {
              if ("choice_84.jpg" %in% image_fdir == TRUE){
                image_path <- paste0(image.folder, "choice_84.jpg")
                choice84<-magick::image_read(image_path)
                plot(choice84)
                title(main=expression(bold("ENTER 84")))

              }

              if ("choice_36.jpg" %in% image_fdir == TRUE){
                image_path <- paste0(image.folder, "choice_36.jpg")
                choice36<-magick::image_read(image_path)
                plot(choice36)
                title(main=expression(bold("ENTER 36")))

              }

            }

            f<-readline(cat("Which applies to your organism?

            Larger whelk; solid chalky-gray shell, with rough yellowish periostracum;
            low but obvious sculpture consisting of about five spiral cords crossed by about 12
            longitudinal ribs in each whorl; flesh of living animal startling white with black blotches;
            up to 10 cm shell length; common offshore, rarely in littoral (ENTER 84)

            Smaller snails with conspicuous siphonal notch; less than 2.5 cm long (ENTER 36)"))


            if (is.null(dev.list()) == FALSE){
              dev.off()
            }
          } else {
            if (e=="39") {

              if ("choice_40.jpg" %in% image_fdir == TRUE && "choice_43.jpg" %in% image_fdir == TRUE) {
                image_path<- paste0(image.folder, "choice_40.jpg")
                choice40<-magick::image_read(image_path)
                op <- par(mfrow=c(1,2))
                plot(choice40)
                title(main=expression(bold("ENTER 40")), line = -4)

                image_path <- paste0(image.folder, "choice_43.jpg")
                choice43<-magick::image_read(image_path)
                plot(choice43)
                title(main=expression(bold("ENTER 43")), line = -4)

              } else {
                if ("choice_40.jpg" %in% image_fdir == TRUE){
                  image_path <- paste0(image.folder, "choice_40.jpg")
                  choice40<-magick::image_read(image_path)
                  plot(choice40)
                  title(main=expression(bold("ENTER 40")))

                }

                if ("choice_43.jpg" %in% image_fdir == TRUE){
                  image_path <- paste0(image.folder, "choice_43.jpg")
                  choice43<-magick::image_read(image_path)
                  plot(choice43)
                  title(main=expression(bold("ENTER 43")))

                }

              }

              f<-readline(cat("Which applies to your organism?

              Smaller drills, etc. under 4 cm shell length as adults (mostly about 2.5 cm long);
              common in littoral (ENTER 40)

              Larger whelks, all over 6 cm length as adults (may be up to 23 cm long);
              mostly sublittoral and offshore (ENTER 43)"))


              if (is.null(dev.list()) == FALSE){
                dev.off()
              }
            } else {
              if (e=="55") {
                genus_species <- "Caecum cooperi"
                print ("Identification! Caecum cooperi (choice 55)")
              } else {
                if (e=="56") {
                  genus_species <- "Caecum pulchellum"
                  print ("Identification! Caecum pulchellum (choice 56)")
                } else {
                  if (e=="51") {
                    genus_species <- "Crepidula fornicata"
                    print ("Identification! Crepidula fornicata (choice 51)")
                  } else {
                    if (e=="52") {
                      genus_species <- "Crepidula plana"
                      print ("Identification! Crepidula plana (choice 52)")
                    } else {
                      if (e=="53") {
                        genus_species <- "Crepidula convexa"
                        print ("Identification! Crepidula convexa (choice 53)")
                      } else {
                        if (e=="11") {

                          if ("choice_57.jpg" %in% image_fdir == TRUE && "choice_58.jpg" %in% image_fdir == TRUE) {
                            image_path<- paste0(image.folder, "choice_57.jpg")
                            choice57<-magick::image_read(image_path)
                            op <- par(mfrow=c(1,2))
                            plot(choice57)
                            title(main=expression(bold("ENTER 57")), line = -4)

                            image_path <- paste0(image.folder, "choice_58.jpg")
                            choice58<-magick::image_read(image_path)
                            plot(choice58)
                            title(main=expression(bold("ENTER 58")), line = -4)

                          } else {
                            if ("choice_57.jpg" %in% image_fdir == TRUE){
                              image_path <- paste0(image.folder, "choice_57.jpg")
                              choice57<-magick::image_read(image_path)
                              plot(choice57)
                              title(main=expression(bold("ENTER 57")))

                            }

                            if ("choice_58.jpg" %in% image_fdir == TRUE){
                              image_path <- paste0(image.folder, "choice_58.jpg")
                              choice58<-magick::image_read(image_path)
                              plot(choice58)
                              title(main=expression(bold("ENTER 58")))

                            }

                          }

                          f<-readline(cat("Which applies to your organism?

                          With internal saucer-shaped horny shell; extensible mantle lobes used for swimming;
                          the only species of sea hare recorded in this area; up to 20 cm long (ENTER 57)

                          With fragile, glassy, bean-shaped turbinate shell within mantle;
                          living animal characteristically exhibiting four fleshy lobes when viewed dorsally;
                          up to 13 mm long (ENTER 58)"))


                          if (is.null(dev.list()) == FALSE){
                            dev.off()
                          }
                        } else {
                          if (e=="12") {

                            if ("choice_59.jpg" %in% image_fdir == TRUE && "choice_13.jpg" %in% image_fdir == TRUE) {
                              image_path<- paste0(image.folder, "choice_59.jpg")
                              choice59<-magick::image_read(image_path)
                              op <- par(mfrow=c(1,2))
                              plot(choice59)
                              title(main=expression(bold("ENTER 59")), line = -4)

                              image_path <- paste0(image.folder, "choice_13.jpg")
                              choice13<-magick::image_read(image_path)
                              plot(choice13)
                              title(main=expression(bold("ENTER 13")), line = -4)

                            } else {
                              if ("choice_59.jpg" %in% image_fdir == TRUE){
                                image_path <- paste0(image.folder, "choice_59.jpg")
                                choice59<-magick::image_read(image_path)
                                plot(choice59)
                                title(main=expression(bold("ENTER 59")))

                              }

                              if ("choice_13.jpg" %in% image_fdir == TRUE){
                                image_path <- paste0(image.folder, "choice_13.jpg")
                                choice13<-magick::image_read(image_path)
                                plot(choice13)
                                title(main=expression(bold("ENTER 13")))

                              }

                            }

                            f<-readline(cat("Which applies to your organism?

                            Shell visible externally, with a prominent spire and ornamented with
                            fine spiral rows of dots at level of aperture;
                            shell up to 8 mm long (ENTER 59)

                            Shells not usually visible externally when animals are active;
                            spire low or absent; thin, glassy shell lacking ornamentation;
                            bubble-shell tectibranchs (Order Cephalaspidea) (ENTER 13)"))


                            if (is.null(dev.list()) == FALSE){
                              dev.off()
                            }
                          } else {
                            if (e=="17") {

                              if ("choice_18.jpg" %in% image_fdir == TRUE && "choice_20.jpg" %in% image_fdir == TRUE) {
                                image_path<- paste0(image.folder, "choice_18.jpg")
                                choice18<-magick::image_read(image_path)
                                op <- par(mfrow=c(1,2))
                                plot(choice18)
                                title(main=expression(bold("ENTER 18")), line = -4)

                                image_path <- paste0(image.folder, "choice_20.jpg")
                                choice20<-magick::image_read(image_path)
                                plot(choice20)
                                title(main=expression(bold("ENTER 20")), line = -4)

                              } else {
                                if ("choice_18.jpg" %in% image_fdir == TRUE){
                                  image_path <- paste0(image.folder, "choice_18.jpg")
                                  choice18<-magick::image_read(image_path)
                                  plot(choice18)
                                  title(main=expression(bold("ENTER 18")))

                                }

                                if ("choice_20.jpg" %in% image_fdir == TRUE){
                                  image_path <- paste0(image.folder, "choice_20.jpg")
                                  choice20<-magick::image_read(image_path)
                                  plot(choice20)
                                  title(main=expression(bold("ENTER 20")))

                                }

                              }

                              f<-readline(cat("Which applies to your organism?

    Shell umbilicus clearly open (fig. 10) (ENTER 18)

    Shell umbilicus totally or nearly occluded by callus (fig. 11) (ENTER 20)"))


                              if (is.null(dev.list()) == FALSE){
                                dev.off()
                              }
                            } else {
                              if (e=="23") {

                                if ("choice_24.jpg" %in% image_fdir == TRUE && "choice_70.jpg" %in% image_fdir == TRUE) {
                                  image_path<- paste0(image.folder, "choice_24.jpg")
                                  choice24<-magick::image_read(image_path)
                                  op <- par(mfrow=c(1,2))
                                  plot(choice24)
                                  title(main=expression(bold("ENTER 24")), line = -4)

                                  image_path <- paste0(image.folder, "choice_70.jpg")
                                  choice70<-magick::image_read(image_path)
                                  plot(choice70)
                                  title(main=expression(bold("ENTER 70")), line = -4)

                                } else {
                                  if ("choice_24.jpg" %in% image_fdir == TRUE){
                                    image_path <- paste0(image.folder, "choice_24.jpg")
                                    choice24<-magick::image_read(image_path)
                                    plot(choice24)
                                    title(main=expression(bold("ENTER 24")))

                                  }

                                  if ("choice_70.jpg" %in% image_fdir == TRUE){
                                    image_path <- paste0(image.folder, "choice_70.jpg")
                                    choice70<-magick::image_read(image_path)
                                    plot(choice70)
                                    title(main=expression(bold("ENTER 70")))

                                  }

                                }

                                f<-readline(cat("Which applies to your organism?

                                Shells coiled dextrally (ENTER 24)

                                Shell coiled sinistrally;
                                10-12 whorls, dark brown with three spiral rows of prominent beads;
                                up to 6 mm long (ENTER 70)"))


                                if (is.null(dev.list()) == FALSE){
                                  dev.off()
                                }
                              } else {
                                if (e=="29") {

                                  if ("choice_30.jpg" %in% image_fdir == TRUE && "choice_78.jpg" %in% image_fdir == TRUE) {
                                    image_path<- paste0(image.folder, "choice_30.jpg")
                                    choice30<-magick::image_read(image_path)
                                    op <- par(mfrow=c(1,2))
                                    plot(choice30)
                                    title(main=expression(bold("ENTER 30")), line = -4)

                                    image_path <- paste0(image.folder, "choice_78.jpg")
                                    choice78<-magick::image_read(image_path)
                                    plot(choice78)
                                    title(main=expression(bold("ENTER 78")), line = -4)

                                  } else {
                                    if ("choice_30.jpg" %in% image_fdir == TRUE){
                                      image_path <- paste0(image.folder, "choice_30.jpg")
                                      choice30<-magick::image_read(image_path)
                                      plot(choice30)
                                      title(main=expression(bold("ENTER 30")))

                                    }

                                    if ("choice_78.jpg" %in% image_fdir == TRUE){
                                      image_path <- paste0(image.folder, "choice_78.jpg")
                                      choice78<-magick::image_read(image_path)
                                      plot(choice78)
                                      title(main=expression(bold("ENTER 78")))

                                    }

                                  }

                                  f<-readline(cat("Which applies to your organism?

                                  Medium sized (up to 3 cm);
                                  coarser shells, lacking umbilicus or apertural groove;
                                  adults rarely translucent (ENTER 30)

                                  Very small, fragile, smooth shell with groove in inner lip extending
                                  into chink-like umbilicus;
                                  color variable from pink to brown with purple or dark brown markings;
                                  up to 8 mm long (ENTER 78)"))


                                  if (is.null(dev.list()) == FALSE){
                                    dev.off()
                                  }
                                } else {
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
              }
            }
          }
        }
      }
    }
  }

  if (f=="19") {
    family<-"Naticidae"
    print("Your organism is in the Naticidae family!")

    if ("choice_65.jpg" %in% image_fdir == TRUE && "choice_66.jpg" %in% image_fdir == TRUE) {
      image_path<- paste0(image.folder, "choice_65.jpg")
      choice65<-magick::image_read(image_path)
      op <- par(mfrow=c(1,2))
      plot(choice65)
      title(main=expression(bold("ENTER 65")), line = -4)

      image_path <- paste0(image.folder, "choice_66.jpg")
      choice66<-magick::image_read(image_path)
      plot(choice66)
      title(main=expression(bold("ENTER 66")), line = -4)

    } else {
      if ("choice_65.jpg" %in% image_fdir == TRUE){
        image_path <- paste0(image.folder, "choice_65.jpg")
        choice65<-magick::image_read(image_path)
        plot(choice65)
        title(main=expression(bold("ENTER 65")))

      }

      if ("choice_66.jpg" %in% image_fdir == TRUE){
        image_path <- paste0(image.folder, "choice_66.jpg")
        choice66<-magick::image_read(image_path)
        plot(choice66)
        title(main=expression(bold("ENTER 66")))

      }

    }

    g<-readline(cat("Which applies to your organism?

    Coarse heavy shell, with no callus; brownish-gray; often with attached algal filaments;
    shell up to 12 cm diameter; common (ENTER 65)

    Thin clean shell, with inconspicuous white callus on inner lip;
    light brown or white with three characteristic rows of squarish, dark-brown spots on last whorl;
    up to 13 mm shell diameter; less common (ENTER 66)"))


    if (is.null(dev.list()) == FALSE){
      dev.off()
    }
  } else {
    if (f=="64") {
      family<-"Naticidae"
      genus_species <- "Polinices immaculatus"
      print("Your organism is in the Naticidae family!")
      print ("Identification! Polinices immaculatus (choice 64)")} else {
      if (f=="67") {
        family<-"Naticidae"
        genus_species <- "Polinices duplicatus"
        print("Your organism is in the Naticidae family!")
        print ("Identification! Polinices duplicatus (choice 67)")
      } else {
        if (f=="21") {
          family<-"Naticidae"
          print("Your organism is in the Naticidae family!")

          if ("choice_68.jpg" %in% image_fdir == TRUE && "choice_69.jpg" %in% image_fdir == TRUE) {
            image_path<- paste0(image.folder, "choice_68.jpg")
            choice68<-magick::image_read(image_path)
            op <- par(mfrow=c(1,2))
            plot(choice68)
            title(main=expression(bold("ENTER 68")), line = -4)

            image_path <- paste0(image.folder, "choice_69.jpg")
            choice69<-magick::image_read(image_path)
            plot(choice69)
            title(main=expression(bold("ENTER 69")), line = -4)

          } else {
            if ("choice_68.jpg" %in% image_fdir == TRUE){
              image_path <- paste0(image.folder, "choice_68.jpg")
              choice68<-magick::image_read(image_path)
              plot(choice68)
              title(main=expression(bold("ENTER 68")))

            }

            if ("choice_69.jpg" %in% image_fdir == TRUE){
              image_path <- paste0(image.folder, "choice_69.jpg")
              choice69<-magick::image_read(image_path)
              plot(choice69)
              title(main=expression(bold("ENTER 69")))

            }

          }

          g<-readline(cat("Which applies to your organism?

          Larger globose shell; polished white flat callus always completely sealing over umbilicus;
          up to 3.8 cm shell height (ENTER 68)

          Smaller ovate shell; white callus usually leaving open chink at umbilicus;
          shell usually with faint bands of light brown; up to 8 mm shell height (ENTER 69)"))


          if (is.null(dev.list()) == FALSE){
            dev.off()
          }
        } else {
          if (f=="84") {
            family<-"Buccinidae"
            genus_species <- "Buccinum undatum"
            print("Your organism is in the Buccinidae family!")
            print ("Identification! Buccinum undatum (choice 84)")
          } else {
            if (f=="36") {

              if ("choice_85.jpg" %in% image_fdir == TRUE && "choice_37.jpg" %in% image_fdir == TRUE) {
                image_path<- paste0(image.folder, "choice_85.jpg")
                choice85<-magick::image_read(image_path)
                op <- par(mfrow=c(1,2))
                plot(choice85)
                title(main=expression(bold("ENTER 85")), line = -4)

                image_path <- paste0(image.folder, "choice_37.jpg")
                choice37<-magick::image_read(image_path)
                plot(choice37)
                title(main=expression(bold("ENTER 37")), line = -4)

              } else {
                if ("choice_85.jpg" %in% image_fdir == TRUE){
                  image_path <- paste0(image.folder, "choice_85.jpg")
                  choice85<-magick::image_read(image_path)
                  plot(choice85)
                  title(main=expression(bold("ENTER 85")))

                }

                if ("choice_37.jpg" %in% image_fdir == TRUE){
                  image_path <- paste0(image.folder, "choice_37.jpg")
                  choice37<-magick::image_read(image_path)
                  plot(choice37)
                  title(main=expression(bold("ENTER 37")))

                }

              }

              g<-readline(cat("Which applies to your organism?

              Stout 'dirty' shell often eroded; dark brown or black with neither suture nor sculpture obvious;
              often with adherent debris or organisms; up to 2.5 cm long;
              often abundant on mud flats intertidally (ENTER 85)

              Clean shells with obvious sculpture (ENTER 37)"))


              if (is.null(dev.list()) == FALSE){
                dev.off()
              }
            } else {
              if (f=="40") {

                if ("choice_90.jpg" %in% image_fdir == TRUE && "choice_41.jpg" %in% image_fdir == TRUE) {
                  image_path<- paste0(image.folder, "choice_90.jpg")
                  choice90<-magick::image_read(image_path)
                  op <- par(mfrow=c(1,2))
                  plot(choice90)
                  title(main=expression(bold("ENTER 90")), line = -4)

                  image_path <- paste0(image.folder, "choice_41.jpg")
                  choice41<-magick::image_read(image_path)
                  plot(choice41)
                  title(main=expression(bold("ENTER 41")), line = -4)

                } else {
                  if ("choice_90.jpg" %in% image_fdir == TRUE){
                    image_path <- paste0(image.folder, "choice_90.jpg")
                    choice90<-magick::image_read(image_path)
                    plot(choice90)
                    title(main=expression(bold("ENTER 90")))

                  }

                  if ("choice_41.jpg" %in% image_fdir == TRUE){
                    image_path <- paste0(image.folder, "choice_41.jpg")
                    choice41<-magick::image_read(image_path)
                    plot(choice41)
                    title(main=expression(bold("ENTER 41")))

                  }

                }

                g<-readline(cat("Which applies to your organism?

                Relatively thin shelled with 6-8 globose whorls in adult; gray with greenish periostracum;
                no obvious shell sculpture; siphonal canal of moderate length bent back from aperture;
                up to 2.5 cm long (ENTER 90)

                Stout-shelled drills with obvious shell sculpture (ENTER 41)"))


                if (is.null(dev.list()) == FALSE){
                  dev.off()
                }
              } else {
                if (f=="43") {

                  if ("choice_44.jpg" %in% image_fdir == TRUE && "choice_46.jpg" %in% image_fdir == TRUE) {
                    image_path<- paste0(image.folder, "choice_44.jpg")
                    choice44<-magick::image_read(image_path)
                    op <- par(mfrow=c(1,2))
                    plot(choice44)
                    title(main=expression(bold("ENTER 44")), line = -4)

                    image_path <- paste0(image.folder, "choice_46.jpg")
                    choice46<-magick::image_read(image_path)
                    plot(choice46)
                    title(main=expression(bold("ENTER 46")), line = -4)

                  } else {
                    if ("choice_44.jpg" %in% image_fdir == TRUE){
                      image_path <- paste0(image.folder, "choice_44.jpg")
                      choice44<-magick::image_read(image_path)
                      plot(choice44)
                      title(main=expression(bold("ENTER 44")))

                    }

                    if ("choice_46.jpg" %in% image_fdir == TRUE){
                      image_path <- paste0(image.folder, "choice_46.jpg")
                      choice46<-magick::image_read(image_path)
                      plot(choice46)
                      title(main=expression(bold("ENTER 46")))

                    }

                  }

                  g<-readline(cat("Which applies to your organism?

                  Combined length of aperture and siphonal canal about one half shell length (ENTER 44)

                  Combined length of aperture and siphonal canal markedly greater than one half shell length
                  (ENTER 46)"))


                  if (is.null(dev.list()) == FALSE){
                    dev.off()
                  }
                } else {
                  if (f=="57") {
                    family<-"Aplysiidae"
                    genus_species <- "Aplysia willcoxi"
                    print("Your organism is in the Aplysiidae family!")
                    print ("Identification! Aplysia willcoxi (choice 57)")
                  } else {
                    if (f=="58") {
                      family<-"Philinidae"
                      genus_species <- "Philine lima"
                      print("Your organism is in the Philinidae family!")
                      print ("Identification! Philine lima (choice 58)")
                    } else {
                      if (f=="59") {
                        family<-"Acteonidae"
                        genus_species <- "Acteon punctostriatus"
                        print("Your organism is in the Acteonidae family!")
                        print ("Identification! Acteon punctostriatus (choice 59)")
                      } else {
                        if (f=="13") {

                          if ("choice_14.jpg" %in% image_fdir == TRUE && "choice_15.jpg" %in% image_fdir == TRUE) {
                            image_path<- paste0(image.folder, "choice_14.jpg")
                            choice14<-magick::image_read(image_path)
                            op <- par(mfrow=c(1,2))
                            plot(choice14)
                            title(main=expression(bold("ENTER 14")), line = -4)

                            image_path <- paste0(image.folder, "choice_15.jpg")
                            choice15<-magick::image_read(image_path)
                            plot(choice15)
                            title(main=expression(bold("ENTER 15")), line = -4)

                          } else {
                            if ("choice_14.jpg" %in% image_fdir == TRUE){
                              image_path <- paste0(image.folder, "choice_14.jpg")
                              choice14<-magick::image_read(image_path)
                              plot(choice14)
                              title(main=expression(bold("ENTER 14")))

                            }

                            if ("choice_15.jpg" %in% image_fdir == TRUE){
                              image_path <- paste0(image.folder, "choice_15.jpg")
                              choice15<-magick::image_read(image_path)
                              plot(choice15)
                              title(main=expression(bold("ENTER 15")))

                            }

                          }

                          g<-readline(cat("Which applies to your organism?

                          Shells with a small but elevated spire (ENTER 14)

                          Shells with spire depressed into pit
                          (that is, body whorl completely encloses the rest of the shell) (ENTER 15)"))


                          if (is.null(dev.list()) == FALSE){
                            dev.off()
                          }
                        } else {
                          if (f=="18") {

                            if ("choice_19.jpg" %in% image_fdir == TRUE && "choice_64.jpg" %in% image_fdir == TRUE) {
                              image_path<- paste0(image.folder, "choice_19.jpg")
                              choice19<-magick::image_read(image_path)
                              op <- par(mfrow=c(1,2))
                              plot(choice19)
                              title(main=expression(bold("ENTER 19")), line = -4)

                              image_path <- paste0(image.folder, "choice_64.jpg")
                              choice64<-magick::image_read(image_path)
                              plot(choice64)
                              title(main=expression(bold("ENTER 64")), line = -4)

                            } else {
                              if ("choice_19.jpg" %in% image_fdir == TRUE){
                                image_path <- paste0(image.folder, "choice_19.jpg")
                                choice19<-magick::image_read(image_path)
                                plot(choice19)
                                title(main=expression(bold("ENTER 19")))

                              }

                              if ("choice_64.jpg" %in% image_fdir == TRUE){
                                image_path <- paste0(image.folder, "choice_64.jpg")
                                choice64<-magick::image_read(image_path)
                                plot(choice64)
                                title(main=expression(bold("ENTER 64")))

                              }

                            }

                            g<-readline(cat("Which applies to your organism?

    With no obvious callus on the shell (ENTER 19)

    With obvious ivory-white thickened callus that encroaches on the umbilicus;
    shell white with yellowish periostracum; up to l cm long (ENTER 64) Polinices immaculatus"))


                            if (is.null(dev.list()) == FALSE){
                              dev.off()
                            }
                          } else {
                            if (f=="20") {

                              if ("choice_67.jpg" %in% image_fdir == TRUE && "choice_21.jpg" %in% image_fdir == TRUE) {
                                image_path<- paste0(image.folder, "choice_67.jpg")
                                choice67<-magick::image_read(image_path)
                                op <- par(mfrow=c(1,2))
                                plot(choice67)
                                title(main=expression(bold("ENTER 67")), line = -4)

                                image_path <- paste0(image.folder, "choice_21.jpg")
                                choice21<-magick::image_read(image_path)
                                plot(choice21)
                                title(main=expression(bold("ENTER 21")), line = -4)

                              } else {
                                if ("choice_67.jpg" %in% image_fdir == TRUE){
                                  image_path <- paste0(image.folder, "choice_67.jpg")
                                  choice67<-magick::image_read(image_path)
                                  plot(choice67)
                                  title(main=expression(bold("ENTER 67")))

                                }

                                if ("choice_21.jpg" %in% image_fdir == TRUE){
                                  image_path <- paste0(image.folder, "choice_21.jpg")
                                  choice21<-magick::image_read(image_path)
                                  plot(choice21)
                                  title(main=expression(bold("ENTER 21")))

                                }

                              }

                              g<-readline(cat("Which applies to your organism?

      Shell considerably wider than high, with flat spire; clean bluish-gray,
      with obvious purple (or more rarely brown or pink) callus almost completely occluding umbilicus;
      up to 7.5 cm diameter; often abundant intertidally and in shallow water (ENTER 67)

      Shells slightly higher than wide with obvious spire; white with pale brown periostracum;
      white callus closing or nearly closing umbilicus;
      opercula more calcified than other naticids above;
      smaller species, less common and usually subtidal (ENTER 21)"))


                              if (is.null(dev.list()) == FALSE){
                                dev.off()
                              }
                            } else {
                              if (f=="24") {

                                if ("choice_71.jpg" %in% image_fdir == TRUE && "choice_25.jpg" %in% image_fdir == TRUE) {
                                  image_path<- paste0(image.folder, "choice_71.jpg")
                                  choice71<-magick::image_read(image_path)
                                  op <- par(mfrow=c(1,2))
                                  plot(choice71)
                                  title(main=expression(bold("ENTER 71")), line = -4)

                                  image_path <- paste0(image.folder, "choice_25.jpg")
                                  choice25<-magick::image_read(image_path)
                                  plot(choice25)
                                  title(main=expression(bold("ENTER 25")), line = -4)

                                } else {
                                  if ("choice_71.jpg" %in% image_fdir == TRUE){
                                    image_path <- paste0(image.folder, "choice_71.jpg")
                                    choice71<-magick::image_read(image_path)
                                    plot(choice71)
                                    title(main=expression(bold("ENTER 71")))

                                  }

                                  if ("choice_25.jpg" %in% image_fdir == TRUE){
                                    image_path <- paste0(image.folder, "choice_25.jpg")
                                    choice25<-magick::image_read(image_path)
                                    plot(choice25)
                                    title(main=expression(bold("ENTER 25")))

                                  }

                                }

                                g<-readline(cat("Which applies to your organism?
                                About 11 markedly-globose whorls, expanding rapidly to give a conical shell;
                                each whorl bearing about 16 strong longitudinal ribs;
                                circular aperture with thickened lip; up to 2.5 cm long;
                                the only true wentletrap recorded in this area (ENTER 71)

                                Whorls not markedly globose; whole shell awl- or spindle-shaped;
                                mostly under 15 mm shell length (ENTER 25)"))


                                if (is.null(dev.list()) == FALSE){
                                  dev.off()
                                }
                              } else {
                                if (f=="70") {
                                  family<-"Triphoridae"
                                  genus_species <- "Triphora nigrocincta"
                                  print("Your organism is in the Triphoridae family!")
                                  print ("Identification! Triphora nigrocincta (choice 70)")
                                } else {
                                  if (f=="30") {

                                    family<-"Littorinidae"
                                    print("Your organism is in the Littorinidae family!")

                                    if ("choice_79.jpg" %in% image_fdir == TRUE && "choice_80.jpg" %in% image_fdir == TRUE && "choice_81.jpg" %in% image_fdir == TRUE) {
                                      image_path<- paste0(image.folder, "choice_79.jpg")
                                      choice79<-magick::image_read(image_path)
                                      op <- par(mfrow=c(2,2))
                                      plot(choice79)
                                      title(main=expression( bold("ENTER 79")), line = -4)

                                      image_path <- paste0(image.folder, "choice_80.jpg")
                                      choice80<-magick::image_read(image_path)
                                      plot(choice80)
                                      title(main=expression(bold("ENTER 80")), line = -4)

                                      image_path <- paste0(image.folder, "choice_81.jpg")
                                      choice81<-magick::image_read(image_path)
                                      plot(choice81)
                                      title(main=expression(bold("ENTER 81")), line = -4)

                                    } else {
                                      if ("choice_79.jpg" %in% image_fdir == TRUE && "choice_80.jpg" %in% image_fdir == TRUE) {
                                        image_path<- paste0(image.folder, "choice_79.jpg")
                                        choice79<-magick::image_read(image_path)
                                        op <- par(mfrow=c(1,2))
                                        plot(choice79)
                                        title(main=expression( bold("ENTER 79")), line = -4)

                                        image_path <- paste0(image.folder, "choice_80.jpg")
                                        choice80<-magick::image_read(image_path)
                                        plot(choice80)
                                        title(main=expression(bold("ENTER 80")), line = -4)
                                      }

                                      if ("choice_79.jpg" %in% image_fdir == TRUE && "choice_81.jpg" %in% image_fdir == TRUE) {
                                        image_path<- paste0(image.folder, "choice_79.jpg")
                                        choice79<-magick::image_read(image_path)
                                        op <- par(mfrow=c(1,2))
                                        plot(choice79)
                                        title(main=expression( bold("ENTER 79")), line = -4)

                                        image_path <- paste0(image.folder, "choice_81.jpg")
                                        choice81<-magick::image_read(image_path)
                                        plot(choice81)
                                        title(main=expression(bold("ENTER 81")), line = -4)
                                      }

                                      if ("choice_80.jpg" %in% image_fdir == TRUE && "choice_81.jpg" %in% image_fdir == TRUE) {
                                        image_path<- paste0(image.folder, "choice_80.jpg")
                                        choice80<-magick::image_read(image_path)
                                        op <- par(mfrow=c(1,2))
                                        plot(choice80)
                                        title(main=expression( bold("ENTER 80")), line = -4)

                                        image_path <- paste0(image.folder, "choice_81.jpg")
                                        choice81<-magick::image_read(image_path)
                                        plot(choice81)
                                        title(main=expression(bold("ENTER 81")), line = -4)
                                      } else {
                                        if ("choice_79.jpg" %in% image_fdir == TRUE){
                                          image_path <- paste0(image.folder, "choice_79.jpg")
                                          choice79<-magick::image_read(image_path)
                                          plot(choice79)
                                          title(main=expression(bold("ENTER 79")))

                                        }
                                        if ("choice_80.jpg" %in% image_fdir == TRUE){
                                          image_path <- paste0(image.folder, "choice_80.jpg")
                                          choice80<-magick::image_read(image_path)
                                          plot(choice80)
                                          title(main=expression(bold("ENTER 80")))

                                        }
                                        if ("choice_81.jpg" %in% image_fdir == TRUE){
                                          image_path <- paste0(image.folder, "choice_81.jpg")
                                          choice81<-magick::image_read(image_path)
                                          plot(choice81)
                                          title(main=expression(bold("ENTER 81")))

                                        }
                                      }
                                    }

                                    g<-readline(cat("Which applies to your organism?

                                    Spire flattened; shell very smooth and shiny;
                                    color variable but commonly uniform, clear yellow-orange;
                                    often banded; up to 1 cm across;
                                    in lower littoral associated with fucoid seaweeds (ENTER 79)

                                    Moderate spire, expanding more rapidly after first tiny whorls with raised
                                    spiral threads when young; usually black when young;
                                    variable but dull as adult; with planktonic larvae (thus apical shell is
                                    small and, if not eroded in adult, is very sharply pointed);
                                    up to 3 cm across;
                                    the commonest and most abundant periwinkle at all tidal levels (ENTER 80)

                                    Obvious but variable spire; shell rough with irregular raised lines;
                                    color variable but commonly greenish-yellow;
                                    viviparous (thus apical shell relatively large); up to 13 mm long;
                                    higher littoral (fig. 14) (ENTER 81)"))


                                    if (is.null(dev.list()) == FALSE){
                                      dev.off()
                                    }
                                  } else {
                                    if (f=="78") {
                                      family<-"Littorinidae"
                                      genus_species <- "Lacuna vincta"
                                      print("Your organism is in the Littorinidae family!")
                                      print ("Identification! Lacuna vincta (choice 78)")
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
  }

  if (g=="65") {
    genus_species <- "Lunatia heros"
    print ("Identification! Lunatia heros (choice 65)")} else {
    if (g=="66") {
        genus_species <- "Lunatia triseriata"
        print ("Identification! Lunatia triseriata (choice 66)")} else {
      if (g=="68") {
            genus_species <- "Natica clausa"
            print ("Identification! Natica clausa (choice 68)")} else {
        if (g=="69") {
                genus_species <- "Natica pusilla"
                print ("Identification! Natica pusilla (choice 69)")} else {
          if (g=="85") {
                    family<-"Nassariidae"
                    genus_species <- "Nassarius obsoletus"
                    print("Your organism is in the Nassariidae family!")
                    print ("Identification! Nassarius obsoletus (choice 85)")} else {
            if (g=="37") {

              if ("choice_86.jpg" %in% image_fdir == TRUE && "choice_87.jpg" %in% image_fdir == TRUE && "choice_38.jpg" %in% image_fdir == TRUE) {
                image_path<- paste0(image.folder, "choice_86.jpg")
                choice86<-magick::image_read(image_path)
                op <- par(mfrow=c(2,2))
                plot(choice86)
                title(main=expression( bold("ENTER 86")), line = -4)

                image_path <- paste0(image.folder, "choice_87.jpg")
                choice87<-magick::image_read(image_path)
                plot(choice87)
                title(main=expression(bold("ENTER 87")), line = -4)

                image_path <- paste0(image.folder, "choice_38.jpg")
                choice38<-magick::image_read(image_path)
                plot(choice38)
                title(main=expression(bold("ENTER 38")), line = -4)

              } else {
                if ("choice_86.jpg" %in% image_fdir == TRUE && "choice_87.jpg" %in% image_fdir == TRUE) {
                  image_path<- paste0(image.folder, "choice_86.jpg")
                  choice86<-magick::image_read(image_path)
                  op <- par(mfrow=c(1,2))
                  plot(choice86)
                  title(main=expression( bold("ENTER 86")), line = -4)

                  image_path <- paste0(image.folder, "choice_87.jpg")
                  choice87<-magick::image_read(image_path)
                  plot(choice87)
                  title(main=expression(bold("ENTER 87")), line = -4)
                }

                if ("choice_86.jpg" %in% image_fdir == TRUE && "choice_38.jpg" %in% image_fdir == TRUE) {
                  image_path<- paste0(image.folder, "choice_86.jpg")
                  choice86<-magick::image_read(image_path)
                  op <- par(mfrow=c(1,2))
                  plot(choice86)
                  title(main=expression( bold("ENTER 86")), line = -4)

                  image_path <- paste0(image.folder, "choice_38.jpg")
                  choice38<-magick::image_read(image_path)
                  plot(choice38)
                  title(main=expression(bold("ENTER 38")), line = -4)
                }

                if ("choice_87.jpg" %in% image_fdir == TRUE && "choice_38.jpg" %in% image_fdir == TRUE) {
                  image_path<- paste0(image.folder, "choice_87.jpg")
                  choice87<-magick::image_read(image_path)
                  op <- par(mfrow=c(1,2))
                  plot(choice87)
                  title(main=expression( bold("ENTER 87")), line = -4)

                  image_path <- paste0(image.folder, "choice_38.jpg")
                  choice38<-magick::image_read(image_path)
                  plot(choice38)
                  title(main=expression(bold("ENTER 38")), line = -4)
                } else {
                  if ("choice_86.jpg" %in% image_fdir == TRUE){
                    image_path <- paste0(image.folder, "choice_86.jpg")
                    choice86<-magick::image_read(image_path)
                    plot(choice86)
                    title(main=expression(bold("ENTER 86")))

                  }
                  if ("choice_87.jpg" %in% image_fdir == TRUE){
                    image_path <- paste0(image.folder, "choice_87.jpg")
                    choice87<-magick::image_read(image_path)
                    plot(choice87)
                    title(main=expression(bold("ENTER 87")))

                  }
                  if ("choice_38.jpg" %in% image_fdir == TRUE){
                    image_path <- paste0(image.folder, "choice_38.jpg")
                    choice38<-magick::image_read(image_path)
                    plot(choice38)
                    title(main=expression(bold("ENTER 38")))

                  }
                }
              }

              h<-readline(cat("Which applies to your organism?

              Essentially conical shell with 8-9 whorls in adult,
              bearing about five spiral rows of ranked beads giving waffle-like pattern;
              off- white in color; up to 2 cm long (fig. 13); usually living on sand (ENTER 86)

              Squat, solid pear-shaped shell with 4-5 whorls in adult,
              each bearing about 12 pronounced longitudinal folds; off-white in color; up to 13 mm long;
              less common in this area, living in muddy sand (ENTER 87)

              Slimmer spindle shaped shells, with 6-7 whorls in adult, with less complex sculpture;
              up to 18 mm long (ENTER 38)"))


              if (is.null(dev.list()) == FALSE){
                dev.off()
              }
            } else {
              if (g=="90") {
                          family<-"Colidae"
                          genus_species <- "Colus pygmaeus"
                          print("Your organism is in the Colidae family!")
                          print ("Identification! Colus pygmaeus (choice 90)")} else {
                if (g=="41") {

                  if ("choice_91.jpg" %in% image_fdir == TRUE && "choice_42.jpg" %in% image_fdir == TRUE) {
                    image_path<- paste0(image.folder, "choice_91.jpg")
                    choice91<-magick::image_read(image_path)
                    op <- par(mfrow=c(1,2))
                    plot(choice91)
                    title(main=expression(bold("ENTER 91")), line = -4)

                    image_path <- paste0(image.folder, "choice_42.jpg")
                    choice42<-magick::image_read(image_path)
                    plot(choice42)
                    title(main=expression(bold("ENTER 42")), line = -4)

                  } else {
                    if ("choice_91.jpg" %in% image_fdir == TRUE){
                      image_path <- paste0(image.folder, "choice_91.jpg")
                      choice91<-magick::image_read(image_path)
                      plot(choice91)
                      title(main=expression(bold("ENTER 91")))

                    }

                    if ("choice_42.jpg" %in% image_fdir == TRUE){
                      image_path <- paste0(image.folder, "choice_42.jpg")
                      choice42<-magick::image_read(image_path)
                      plot(choice42)
                      title(main=expression(bold("ENTER 42")))

                    }

                  }

                  h<-readline(cat("Which applies to your organism?

                  With five angular whorls and deep suture, giving sharp 'shoulders' to spire;
                  relatively long siphonal canal which is almost closed over;
                  up to 2.5 cm long (fig. 12) (ENTER 91)

                  With rounded but ornamented whorls; siphonal canal relatively short and open (ENTER 42)"))


                  if (is.null(dev.list()) == FALSE){
                    dev.off()
                  }
                } else {
                  if (g=="44") {

                    if ("choice_94.jpg" %in% image_fdir == TRUE && "choice_45.jpg" %in% image_fdir == TRUE) {
                      image_path<- paste0(image.folder, "choice_94.jpg")
                      choice94<-magick::image_read(image_path)
                      op <- par(mfrow=c(1,2))
                      plot(choice94)
                      title(main=expression(bold("ENTER 94")), line = -4)

                      image_path <- paste0(image.folder, "choice_45.jpg")
                      choice45<-magick::image_read(image_path)
                      plot(choice45)
                      title(main=expression(bold("ENTER 45")), line = -4)

                    } else {
                      if ("choice_94.jpg" %in% image_fdir == TRUE){
                        image_path <- paste0(image.folder, "choice_94.jpg")
                        choice94<-magick::image_read(image_path)
                        plot(choice94)
                        title(main=expression(bold("ENTER 94")))

                      }

                      if ("choice_45.jpg" %in% image_fdir == TRUE){
                        image_path <- paste0(image.folder, "choice_45.jpg")
                        choice45<-magick::image_read(image_path)
                        plot(choice45)
                        title(main=expression(bold("ENTER 45")))

                      }

                    }

                    h<-readline(cat("Which applies to your organism?

                    Heavy grayish shell with 6-7 whorls as adult,
                    bearing up to ten conspicuous reddish-brown spiral ridges;
                    up to 10 cm long (ENTER 94)

                    Lighter shells with simple globose whorls (ENTER 45)"))


                    if (is.null(dev.list()) == FALSE){
                      dev.off()
                    }
                  } else {
                    if (g=="46") {

                      family<-"Busyconidae"
                      print("Your organism is in the Busyconidae family!")

                      if ("choice_97.jpg" %in% image_fdir == TRUE && "choice_98.jpg" %in% image_fdir == TRUE) {
                        image_path<- paste0(image.folder, "choice_97.jpg")
                        choice97<-magick::image_read(image_path)
                        op <- par(mfrow=c(1,2))
                        plot(choice97)
                        title(main=expression(bold("ENTER 97")), line = -4)

                        image_path <- paste0(image.folder, "choice_98.jpg")
                        choice98<-magick::image_read(image_path)
                        plot(choice98)
                        title(main=expression(bold("ENTER 98")), line = -4)

                      } else {
                        if ("choice_97.jpg" %in% image_fdir == TRUE){
                          image_path <- paste0(image.folder, "choice_97.jpg")
                          choice97<-magick::image_read(image_path)
                          plot(choice97)
                          title(main=expression(bold("ENTER 97")))

                        }

                        if ("choice_98.jpg" %in% image_fdir == TRUE){
                          image_path <- paste0(image.folder, "choice_98.jpg")
                          choice98<-magick::image_read(image_path)
                          plot(choice98)
                          title(main=expression(bold("ENTER 98")))

                        }

                      }

                      h<-readline(cat("Which applies to your organism?

                      Suture channeled giving broad flat shoulders to whorls;
                      up to 20 cm long (ENTER 97)

                      Suture not deeply incised; single row of knobby tubercles on inclined shoulders of whorls;
                      up to 23 cm long (ENTER 98)"))


                      if (is.null(dev.list()) == FALSE){
                        dev.off()
                      }
                    } else {
                      if (g=="14") {

                        if ("choice_60.jpg" %in% image_fdir == TRUE && "choice_61.jpg" %in% image_fdir == TRUE) {
                          image_path<- paste0(image.folder, "choice_60.jpg")
                          choice60<-magick::image_read(image_path)
                          op <- par(mfrow=c(1,2))
                          plot(choice60)
                          title(main=expression(bold("ENTER 60")), line = -4)

                          image_path <- paste0(image.folder, "choice_61.jpg")
                          choice61<-magick::image_read(image_path)
                          plot(choice61)
                          title(main=expression(bold("ENTER 61")), line = -4)

                        } else {
                          if ("choice_60.jpg" %in% image_fdir == TRUE){
                            image_path <- paste0(image.folder, "choice_60.jpg")
                            choice60<-magick::image_read(image_path)
                            plot(choice60)
                            title(main=expression(bold("ENTER 60")))

                          }

                          if ("choice_61.jpg" %in% image_fdir == TRUE){
                            image_path <- paste0(image.folder, "choice_61.jpg")
                            choice61<-magick::image_read(image_path)
                            plot(choice61)
                            title(main=expression(bold("ENTER 61")))

                          }

                        }

                        h<-readline(cat("Which applies to your organism?

                        Stubby, fragile shell with very low spire; white with yellowish- brown staining;
                        columella smooth; up to 3 mm shell length (ENTER 60)

                        Moderately elongate, stronger shell with more obvious spire except when eroded;
                        white with dark rust-brown staining; columella with strong spiral ridge;
                        up to 6 mm shell length (ENTER 61)"))


                        if (is.null(dev.list()) == FALSE){
                          dev.off()
                        }
                      } else {
                        if (g=="15") {

                          if ("choice_62.jpg" %in% image_fdir == TRUE && "choice_63.jpg" %in% image_fdir == TRUE) {
                            image_path<- paste0(image.folder, "choice_62.jpg")
                            choice62<-magick::image_read(image_path)
                            op <- par(mfrow=c(1,2))
                            plot(choice62)
                            title(main=expression(bold("ENTER 62")), line = -4)

                            image_path <- paste0(image.folder, "choice_63.jpg")
                            choice63<-magick::image_read(image_path)
                            plot(choice63)
                            title(main=expression(bold("ENTER 63")), line = -4)

                          } else {
                            if ("choice_62.jpg" %in% image_fdir == TRUE){
                              image_path <- paste0(image.folder, "choice_62.jpg")
                              choice62<-magick::image_read(image_path)
                              plot(choice62)
                              title(main=expression(bold("ENTER 62")))

                            }

                            if ("choice_63.jpg" %in% image_fdir == TRUE){
                              image_path <- paste0(image.folder, "choice_63.jpg")
                              choice63<-magick::image_read(image_path)
                              plot(choice63)
                              title(main=expression(bold("ENTER 63")))

                            }

                          }

                          h<-readline(cat("Which applies to your organism?

                          Larger globose shell; fragile bluish-white; relatively large aperture;
                          up to 13 mm shell length (ENTER 62)

                          Smaller, elongate cylindrical shell; white with brown periostracum;
                          relatively narrow aperture; up to 5 mm shell length (ENTER 63)"))


                          if (is.null(dev.list()) == FALSE){
                            dev.off()
                          }
                        } else {
                          if (g=="19") {
                            family<-"Naticidae"
                            print("Your organism is in the Naticidae family!")

                            if ("choice_65.jpg" %in% image_fdir == TRUE && "choice_66.jpg" %in% image_fdir == TRUE) {
                              image_path<- paste0(image.folder, "choice_65.jpg")
                              choice65<-magick::image_read(image_path)
                              op <- par(mfrow=c(1,2))
                              plot(choice65)
                              title(main=expression(bold("ENTER 65")), line = -4)

                              image_path <- paste0(image.folder, "choice_66.jpg")
                              choice66<-magick::image_read(image_path)
                              plot(choice66)
                              title(main=expression(bold("ENTER 66")), line = -4)

                            } else {
                              if ("choice_65.jpg" %in% image_fdir == TRUE){
                                image_path <- paste0(image.folder, "choice_65.jpg")
                                choice65<-magick::image_read(image_path)
                                plot(choice65)
                                title(main=expression(bold("ENTER 65")))

                              }

                              if ("choice_66.jpg" %in% image_fdir == TRUE){
                                image_path <- paste0(image.folder, "choice_66.jpg")
                                choice66<-magick::image_read(image_path)
                                plot(choice66)
                                title(main=expression(bold("ENTER 66")))

                              }

                            }

                            h<-readline(cat("Which applies to your organism?

    Coarse heavy shell, with no callus; brownish-gray; often with attached algal filaments;
    shell up to 12 cm diameter; common (ENTER 65)

    Thin clean shell, with inconspicuous white callus on inner lip;
    light brown or white with three characteristic rows of squarish, dark-brown spots on last whorl;
    up to 13 mm shell diameter; less common (ENTER 66)"))


                            if (is.null(dev.list()) == FALSE){
                              dev.off()
                            }
                          } else {
                            if (g=="64") {
                              family<-"Naticidae"
                              genus_species <- "Polinices immaculatus"
                              print("Your organism is in the Naticidae family!")
                              print ("Identification! Polinices immaculatus (choice 64)")} else {
                              if (g=="67") {
                                  family<-"Naticidae"
                                  genus_species <- "Polinices duplicatus"
                                  print("Your organism is in the Naticidae family!")
                                  print ("Identification! Polinices duplicatus (choice 67)")
                                } else {
                                if (g=="21") {
                                    family<-"Naticidae"
                                    print("Your organism is in the Naticidae family!")

                                    if ("choice_68.jpg" %in% image_fdir == TRUE && "choice_69.jpg" %in% image_fdir == TRUE) {
                                      image_path<- paste0(image.folder, "choice_68.jpg")
                                      choice68<-magick::image_read(image_path)
                                      op <- par(mfrow=c(1,2))
                                      plot(choice68)
                                      title(main=expression(bold("ENTER 68")), line = -4)

                                      image_path <- paste0(image.folder, "choice_69.jpg")
                                      choice69<-magick::image_read(image_path)
                                      plot(choice69)
                                      title(main=expression(bold("ENTER 69")), line = -4)

                                    } else {
                                      if ("choice_68.jpg" %in% image_fdir == TRUE){
                                        image_path <- paste0(image.folder, "choice_68.jpg")
                                        choice68<-magick::image_read(image_path)
                                        plot(choice68)
                                        title(main=expression(bold("ENTER 68")))

                                      }

                                      if ("choice_69.jpg" %in% image_fdir == TRUE){
                                        image_path <- paste0(image.folder, "choice_69.jpg")
                                        choice69<-magick::image_read(image_path)
                                        plot(choice69)
                                        title(main=expression(bold("ENTER 69")))

                                      }

                                    }

                                    h<-readline(cat("Which applies to your organism?

          Larger globose shell; polished white flat callus always completely sealing over umbilicus;
          up to 3.8 cm shell height (ENTER 68)

          Smaller ovate shell; white callus usually leaving open chink at umbilicus;
          shell usually with faint bands of light brown; up to 8 mm shell height (ENTER 69)"))


                                    if (is.null(dev.list()) == FALSE){
                                      dev.off()
                                    }
                                  } else {
                                  if (g=="71") {
                                      family<-"EPITONIIDAE"
                                      genus_species <- " Epitonium rupicola"
                                      print("Your organism is in the EPITONIIDAE family!")
                                      print ("Identification!  Epitonium rupicola (choice 71)")} else {
                                    if (g=="25") {

                                      if ("choice_26.jpg" %in% image_fdir == TRUE && "choice_27.jpg" %in% image_fdir == TRUE) {
                                        image_path<- paste0(image.folder, "choice_26.jpg")
                                        choice26<-magick::image_read(image_path)
                                        op <- par(mfrow=c(1,2))
                                        plot(choice26)
                                        title(main=expression(bold("ENTER 26")), line = -4)

                                        image_path <- paste0(image.folder, "choice_27.jpg")
                                        choice27<-magick::image_read(image_path)
                                        plot(choice27)
                                        title(main=expression(bold("ENTER 27")), line = -4)

                                      } else {
                                        if ("choice_26.jpg" %in% image_fdir == TRUE){
                                          image_path <- paste0(image.folder, "choice_26.jpg")
                                          choice26<-magick::image_read(image_path)
                                          plot(choice26)
                                          title(main=expression(bold("ENTER 26")))

                                        }

                                        if ("choice_27.jpg" %in% image_fdir == TRUE){
                                          image_path <- paste0(image.folder, "choice_27.jpg")
                                          choice27<-magick::image_read(image_path)
                                          plot(choice27)
                                          title(main=expression(bold("ENTER 27")))

                                        }

                                      }

                                      h<-readline(cat("Which applies to your organism?

                                      Without obvious shell sculpture;
                                      about five somewhat globose whorls separated by a clear suture;
                                      aperture ovate-circular; up to 5 mm in length (ENTER 26)

                                      With obvious shell sculpture of various forms;
                                      more than six somewhat flatter whorls; aperture flattened or rectangular;
                                      up to 18 mm long (ENTER 27)"))


                                      if (is.null(dev.list()) == FALSE){
                                        dev.off()
                                      }
                                    } else {
                                      if (g=="79") {
                                        genus_species <- "Littorina obtusata"
                                        print ("Identification! Littorina obtusata (choice 79)")} else {
                                        if (g=="80") {
                                            genus_species <- "Littorina littorea"
                                            print ("Identification! Littorina littorea (choice 80)")} else {
                                          if (g=="81") {
                                                genus_species <- "Littorina saxatilis"
                                                print ("Identification! Littorina saxatilis (choice 81)")} else {
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
        }
      }
    }
  }

  if (h=="65") {
    genus_species <- "Lunatia heros"
    print ("Identification! Lunatia heros (choice 65)")} else {
    if (h=="66") {
        genus_species <- "Lunatia triseriata"
        print ("Identification! Lunatia triseriata (choice 66)")} else {
      if (h=="68") {
            genus_species <- "Natica clausa"
            print ("Identification! Natica clausa (choice 68)")} else {
        if (h=="69") {
            genus_species <- "Natica pusilla"
            print ("Identification! Natica pusilla (choice 69)")} else {
          if (h=="86") {
                family<-"Nassariidae"
                genus_species <- "Nassarius trivittatus"
                print("Your organism is in the Nassariidae family!")
                print ("Identification! Nassarius trivittatus (choice 86)")} else {
            if (h=="87") {
                    family<-"Nassariidae"
                    genus_species <- "Nassarius vibex"
                    print("Your organism is in the Nassariidae family!")
                    print ("Identification! Nassarius vibex (choice 87)")} else {
              if (h=="38") {

                family<-"Columbellidae"
                print("Your organism is in the Columbellidae family!")

                if ("choice_88.jpg" %in% image_fdir == TRUE && "choice_89.jpg" %in% image_fdir == TRUE) {
                  image_path<- paste0(image.folder, "choice_88.jpg")
                  choice88<-magick::image_read(image_path)
                  op <- par(mfrow=c(1,2))
                  plot(choice88)
                  title(main=expression(bold("ENTER 88")), line = -4)

                  image_path <- paste0(image.folder, "choice_89.jpg")
                  choice89<-magick::image_read(image_path)
                  plot(choice89)
                  title(main=expression(bold("ENTER 89")), line = -4)

                } else {
                  if ("choice_88.jpg" %in% image_fdir == TRUE){
                    image_path <- paste0(image.folder, "choice_88.jpg")
                    choice88<-magick::image_read(image_path)
                    plot(choice88)
                    title(main=expression(bold("ENTER 88")))

                  }

                  if ("choice_89.jpg" %in% image_fdir == TRUE){
                    image_path <- paste0(image.folder, "choice_89.jpg")
                    choice89<-magick::image_read(image_path)
                    plot(choice89)
                    title(main=expression(bold("ENTER 89")))

                  }

                }

                i<-readline(cat("Which applies to your organism?

                With about 12 low rounded longitudinal folds on each whorl; usually dark grayish-brown;
                up to 13 mm long; lower littoral (ENTER 88)

                With about 24 narrow longitudinal folds on each whorl crossed by incised spiral lines;
                usually drab yellow in color; up to 18 mm long; lower littoral and shallow water (ENTER 89)"))


                if (is.null(dev.list()) == FALSE){
                  dev.off()
                }
              } else {
                if (h=="42") {

                  family<-"Muricidae"
                  print("Your organism is in the Muricidae family!")

                  if ("choice_92.jpg" %in% image_fdir == TRUE && "choice_93.jpg" %in% image_fdir == TRUE) {
                    image_path<- paste0(image.folder, "choice_92.jpg")
                    choice92<-magick::image_read(image_path)
                    op <- par(mfrow=c(1,2))
                    plot(choice92)
                    title(main=expression(bold("ENTER 92")), line = -4)

                    image_path <- paste0(image.folder, "choice_93.jpg")
                    choice93<-magick::image_read(image_path)
                    plot(choice93)
                    title(main=expression(bold("ENTER 93")), line = -4)

                  } else {
                    if ("choice_92.jpg" %in% image_fdir == TRUE){
                      image_path <- paste0(image.folder, "choice_92.jpg")
                      choice92<-magick::image_read(image_path)
                      plot(choice92)
                      title(main=expression(bold("ENTER 92")))

                    }

                    if ("choice_93.jpg" %in% image_fdir == TRUE){
                      image_path <- paste0(image.folder, "choice_93.jpg")
                      choice93<-magick::image_read(image_path)
                      plot(choice93)
                      title(main=expression(bold("ENTER 93")))

                    }

                  }

                  i<-readline(cat("Which applies to your organism?

                  Very thick polished white shell with thickened lip (less common color variants:
                  clear yellow, orange-brownish, or striped);
                  about five whorls in adult with rounded spiral ridges giving corrugated appearance;
                  up to 3.5 cm long; less common, on exposed rocky shores (ENTER 92)

                  Thick duller grayish, yellowish, or brown shell; about six whorls in adult,
                  each with 9-12 strong longitudinal ribs crossed by spiral grooves giving knobby appearance;
                  up to 2.5 cm long; the commonest local drill; on all hard substrates (ENTER 93)"))


                  if (is.null(dev.list()) == FALSE){
                    dev.off()
                  }
                } else {
                  if (h=="91") {
                    family<-"Muricidae"
                    genus_species <- "Eupleura caudata"
                    print("Your organism is in the Muricidae family!")
                    print ("Identification! Eupleura caudata (choice 91)")} else {
                    if (h=="45") {

                      family<-"Colidae"
                      print("Your organism is in the Colidae family!")

                      if ("choice_95.jpg" %in% image_fdir == TRUE && "choice_96.jpg" %in% image_fdir == TRUE) {
                        image_path<- paste0(image.folder, "choice_95.jpg")
                        choice95<-magick::image_read(image_path)
                        op <- par(mfrow=c(1,2))
                        plot(choice95)
                        title(main=expression(bold("ENTER 95")), line = -4)

                        image_path <- paste0(image.folder, "choice_96.jpg")
                        choice96<-magick::image_read(image_path)
                        plot(choice96)
                        title(main=expression(bold("ENTER 96")), line = -4)

                      } else {
                        if ("choice_95.jpg" %in% image_fdir == TRUE){
                          image_path <- paste0(image.folder, "choice_95.jpg")
                          choice95<-magick::image_read(image_path)
                          plot(choice95)
                          title(main=expression(bold("ENTER 95")))

                        }

                        if ("choice_96.jpg" %in% image_fdir == TRUE){
                          image_path <- paste0(image.folder, "choice_96.jpg")
                          choice96<-magick::image_read(image_path)
                          plot(choice96)
                          title(main=expression(bold("ENTER 96")))

                        }

                      }

                      i<-readline(cat("Which applies to your organism?

                      Shells more elongate, sharper spired; 6-7 whorls in adult; siphonal canal straight;
                      thin semi-glossy periostracum; up to 13 cm long (ENTER 95)

                      Shells less elongate, blunter spire; 5-6 whorls in adult;
                      siphonal canal usually twisted away from aperture; hairy periostracum;
                      up to 6.5 cm shell length (ENTER 96)"))


                      if (is.null(dev.list()) == FALSE){
                        dev.off()
                      }
                    } else {
                      if (h=="94") {
                          family<-"Buccinidae"
                          genus_species <- "Neptunea decemcostata"
                          print("Your organism is in the Buccinidae family!")
                          print ("Identification! Neptunea decemcostata (choice 94)")} else {
                        if (h=="97") {
                              genus_species <- "Busycon canaliculatum"
                              print ("Identification! Busycon canaliculatum (choice 97)")} else {
                          if (h=="98") {
                                  genus_species <- "Busycon carica"
                                  print ("Identification! Busycon carica (choice 98)")} else {
                            if (h=="60") {
                              family<-"Retusidae"
                              genus_species <- "Retusa obtusa"
                              print("Your organism is in the Retusidae family!")
                              print ("Identification! Retusa obtusa (choice 60)")} else {
                              if (h=="61") {
                                family<-"Cylichnidae"
                                genus_species <- "Retusa canaliculata"
                                print("Your organism is in the Cylichnidae family!")
                                print ("Identification! Retusa canaliculata (choice 61)")} else {
                                if (h=="62") {
                                    family<-"Haminoeidae"
                                    genus_species <- "Haminoea solitaria"
                                    print("Your organism is in the Haminoeidae family!")
                                    print ("Identification! Haminoea solitaria (choice 62)")} else {
                                  if (h=="63") {
                                        family<-"Cylichnidae"
                                        genus_species <- "Cylichna alba"
                                        print("Your organism is in the Cylichnidae family!")
                                        print ("Identification! Cylichna alba (choice 63)  (Note: Other species of bubble-shell tectibranchs may occur in this area.)")} else {
                                    if (h=="26") {

                                      if ("choice_72.jpg" %in% image_fdir == TRUE && "choice_73.jpg" %in% image_fdir == TRUE) {
                                        image_path<- paste0(image.folder, "choice_72.jpg")
                                        choice72<-magick::image_read(image_path)
                                        op <- par(mfrow=c(1,2))
                                        plot(choice72)
                                        title(main=expression(bold("ENTER 72")), line = -4)

                                        image_path <- paste0(image.folder, "choice_73.jpg")
                                        choice73<-magick::image_read(image_path)
                                        plot(choice73)
                                        title(main=expression(bold("ENTER 73")), line = -4)

                                      } else {
                                        if ("choice_72.jpg" %in% image_fdir == TRUE){
                                          image_path <- paste0(image.folder, "choice_72.jpg")
                                          choice72<-magick::image_read(image_path)
                                          plot(choice72)
                                          title(main=expression(bold("ENTER 72")))

                                        }

                                        if ("choice_73.jpg" %in% image_fdir == TRUE){
                                          image_path <- paste0(image.folder, "choice_73.jpg")
                                          choice73<-magick::image_read(image_path)
                                          plot(choice73)
                                          title(main=expression(bold("ENTER 73")))

                                        }

                                      }

                                      i<-readline(cat("Which applies to your organism?

                                      Minute; smooth yellow-brown shells with no markings apart
                                      from growth lines; up to 5 mm long (several species may occur in this area,
                                      including some in brackish waters) (ENTER 72)

                                      Minute; light yellow to brown shell with microscopic spiral sculpture
                                      of incised lines; tiny riblets near suture; up to 2.5 mm long (ENTER 73)"))


                                      if (is.null(dev.list()) == FALSE){
                                        dev.off()
                                      }
                                    } else {
                                      if (h=="27") {

                                        if ("choice_74.jpg" %in% image_fdir == TRUE && "choice_28.jpg" %in% image_fdir == TRUE) {
                                          image_path<- paste0(image.folder, "choice_74.jpg")
                                          choice74<-magick::image_read(image_path)
                                          op <- par(mfrow=c(1,2))
                                          plot(choice74)
                                          title(main=expression(bold("ENTER 74")), line = -4)

                                          image_path <- paste0(image.folder, "choice_28.jpg")
                                          choice28<-magick::image_read(image_path)
                                          plot(choice28)
                                          title(main=expression(bold("ENTER 28")), line = -4)

                                        } else {
                                          if ("choice_74.jpg" %in% image_fdir == TRUE){
                                            image_path <- paste0(image.folder, "choice_74.jpg")
                                            choice74<-magick::image_read(image_path)
                                            plot(choice74)
                                            title(main=expression(bold("ENTER 74")))

                                          }

                                          if ("choice_28.jpg" %in% image_fdir == TRUE){
                                            image_path <- paste0(image.folder, "choice_28.jpg")
                                            choice28<-magick::image_read(image_path)
                                            plot(choice28)
                                            title(main=expression(bold("ENTER 28")))

                                          }

                                        }

                                        i<-readline(cat("Which applies to your organism?

                                        Shell aperture rounded with barely perceptible siphonal notch;
                                        6-8 whorls when adult; about 5 mm long;
                                        common species which can be abundant intertidally (ENTER 74)

                                        Shells with obvious siphonal notch at anterior of aperture;
                                        10-15 whorls when adult; lengths 3 mm to 15 mm;
                                        rarer species, usually subtidal (ENTER 28)"))


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
              }
            }
          }
        }
      }
    }
  }

  if (i=="88") {
    genus_species <- "Anachis avara"
    print ("Identification! Anachis avara (choice 88)")} else {
    if (i=="89") {
        genus_species <- "Anachis translirata"
        print ("Identification! Anachis translirata (choice 89) (Note: Specific identification of minute specimens suspected of being spat (juveniles) of Nassarius spp. or Anachis spp. is aided by comparing them with the apical parts of the shell in known — and not eroded — specimens of all five species")} else {
      if (i=="92") {
            genus_species <- "Thais (= Nucella) lapillus"
            print ("Identification! Thais (= Nucella) lapillus (choice 92)")} else {
        if (i=="93") {
                genus_species <- "Urosalpinx cinerea"
                print ("Identification! Urosalpinx cinerea (choice 93) (Note: Minute specimens suspected of being spat (juveniles) of Eupleura, Thais, or Urosalpinx can best be separated by the relative lengths of their siphonal canals. Comparison with apical parts of the shells of known adults is less useful here).")} else {
          if (i=="95") {
                    genus_species <- "Colus stimpsoni"
                    print ("Identification! Colus stimpsoni (choice 95)")} else {
            if (i=="96") {
                        genus_species <- "Colus pubescens"
                        print ("Identification! Colus pubescens (choice 96)")} else {
              if (i=="72") {
                            family<-"Hydrobiidae"
                            genus_species <- "Hydrobia spp."
                            print("Your organism is in the Hydrobiidae family!")
                            print ("Identification! Hydrobia spp. (choice 72)")} else {
                if (i=="73") {
                                family<-"Rissoidae"
                                genus_species <- "Cingula aculeus"
                                print("Your organism is in the Rissoidae family!")
                                print ("Identification! Cingula aculeus (choice 73)")} else {
                  if (i=="74") {
                                    family<-"Cerithiidae"
                                    genus_species <- "Bittium alternatum"
                                    print("Your organism is in the Cerithiidae family!")
                                    print ("Identification! Bittium alternatum (choice 74)")} else {
                    if (i=="28") {

                      family<-"Cerithiopsidae"
                      print("Your organism is in the Cerithiopsidae family!")

                        if ("choice_75.jpg" %in% image_fdir == TRUE && "choice_76.jpg" %in% image_fdir == TRUE && "choice_77.jpg" %in% image_fdir == TRUE) {
                          image_path<- paste0(image.folder, "choice_75.jpg")
                          choice75<-magick::image_read(image_path)
                          op <- par(mfrow=c(2,2))
                          plot(choice75)
                          title(main=expression( bold("ENTER 75")), line = -4)

                          image_path <- paste0(image.folder, "choice_76.jpg")
                          choice76<-magick::image_read(image_path)
                          plot(choice76)
                          title(main=expression(bold("ENTER 76")), line = -4)

                          image_path <- paste0(image.folder, "choice_77.jpg")
                          choice77<-magick::image_read(image_path)
                          plot(choice77)
                          title(main=expression(bold("ENTER 77")), line = -4)

                        } else {
                          if ("choice_75.jpg" %in% image_fdir == TRUE && "choice_76.jpg" %in% image_fdir == TRUE) {
                            image_path<- paste0(image.folder, "choice_75.jpg")
                            choice75<-magick::image_read(image_path)
                            op <- par(mfrow=c(1,2))
                            plot(choice75)
                            title(main=expression( bold("ENTER 75")), line = -4)

                            image_path <- paste0(image.folder, "choice_76.jpg")
                            choice76<-magick::image_read(image_path)
                            plot(choice76)
                            title(main=expression(bold("ENTER 76")), line = -4)
                          }

                          if ("choice_75.jpg" %in% image_fdir == TRUE && "choice_77.jpg" %in% image_fdir == TRUE) {
                            image_path<- paste0(image.folder, "choice_75.jpg")
                            choice75<-magick::image_read(image_path)
                            op <- par(mfrow=c(1,2))
                            plot(choice75)
                            title(main=expression( bold("ENTER 75")), line = -4)

                            image_path <- paste0(image.folder, "choice_77.jpg")
                            choice77<-magick::image_read(image_path)
                            plot(choice77)
                            title(main=expression(bold("ENTER 77")), line = -4)
                          }

                          if ("choice_76.jpg" %in% image_fdir == TRUE && "choice_77.jpg" %in% image_fdir == TRUE) {
                            image_path<- paste0(image.folder, "choice_76.jpg")
                            choice76<-magick::image_read(image_path)
                            op <- par(mfrow=c(1,2))
                            plot(choice76)
                            title(main=expression( bold("ENTER 76")), line = -4)

                            image_path <- paste0(image.folder, "choice_77.jpg")
                            choice77<-magick::image_read(image_path)
                            plot(choice77)
                            title(main=expression(bold("ENTER 77")), line = -4)
                          } else {
                            if ("choice_75.jpg" %in% image_fdir == TRUE){
                              image_path <- paste0(image.folder, "choice_75.jpg")
                              choice75<-magick::image_read(image_path)
                              plot(choice75)
                              title(main=expression(bold("ENTER 75")))

                            }
                            if ("choice_76.jpg" %in% image_fdir == TRUE){
                              image_path <- paste0(image.folder, "choice_76.jpg")
                              choice76<-magick::image_read(image_path)
                              plot(choice76)
                              title(main=expression(bold("ENTER 76")))

                            }
                            if ("choice_77.jpg" %in% image_fdir == TRUE){
                              image_path <- paste0(image.folder, "choice_77.jpg")
                              choice77<-magick::image_read(image_path)
                              plot(choice77)
                              title(main=expression(bold("ENTER 77")))

                            }
                          }
                        }

                        j<-readline(cat("Which applies to your organism?

                        Minute species; glossy brown; later whorls bearing 2-3 rows of glassy beads;
                        10 whorls when adult; about 3 mm long (ENTER 75)

                        Larger species; more elongate; chocolate brown; flattish whorls,
                        each bearing three rows of distinct raised beads which are lighter in color;
                        14-15 whorls when adult; about 15 mm long (ENTER 76)

                        Larger species; more elongate;
                        flattish whorls each bearing three strong continuous squarish spiral cords;
                        10-12 whorls when adult; about 13 mm long (fig. 15) (ENTER 77)"))


                        if (is.null(dev.list()) == FALSE){
                          dev.off()
                        }


                    } else {
                      if (i=="NA"){

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
  }


  if (j=="75") {
    genus_species <- "Cerithiopsis greeni"
    print ("Identification! Cerithiopsis greeni (choice 75)")} else {
    if (j=="76") {
        genus_species <- "Cerithiopsis subulata"
        print ("Identification! Cerithiopsis subulata (choice 76)")} else {
      if (j=="77") {
            genus_species <- "Seila adamsi"
            print ("Identification! Seila adamsi (choice 77)")} else {
        if (j=="NA"){

              } else {
          print("ERROR! Not a choice!")
        }
      }
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
              Family:", family,
                "
              Genus/Species:", genus_species,
                "
"))

  return("No Errors? Congratulations! You are clearly in your dichotomous key era! Slay!! If you have errors, no worries, it can still be a brat summer! Just rerun the function to begin again!")
}

