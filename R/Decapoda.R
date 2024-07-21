#' Decapoda Key
#'
#' This function allows you to identify your decapod based on their morphology. You should start with an empty
#' folder where you will eventually store photos of the morphological structures you've used to identify your
#' organisms. This function requires a single input: the path to this folder. It's ok if it's empty to begin
#' with, but in order to run the function you do need to provide it's path on your computer. This path should be
#' contained in quotes and end in a forward slash "/". Once you provide the path, you can begin keying your
#' decapod! As you move through the identification, you will need to enter numbers based on the interactive
#' questions. Once you populate images into your folder, you will see them plot side by side on the questions
#' they apply to! Important: when naming images, use the following structure "choice_#.jpg" where the pound sign
#' "#" should be replaced by the number you would choose in the key based on the morphological structure
#' you've photographed. You should have no more than one photo per number.
#'
#' @param image.folder Path to the folder that contains your identification images
#' @return A list of your key choices as well as a species Identification!
#' @export

decapoda<-function(image.folder){

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

  infraorder <- "Infraorder not identified"
  family <- "Family not identified"
  genus_species <- "Identification not found"


a <- readline(cat("Which applies to your organism?

  Small animal of shrimp-like form;
  abdomen well developed, with tail fan;
  pleopods used for swimming;
  pleura [side plates] of second abdominal segment overlapping those of first segment — the typical
  shrimps and prawns
  (ENTER 1)

  Animals of heavier form, with a well developed abdomen;
  pleopods not used for swimming;
  thoracic legs adapted for walking;
  chelipeds large and strong

    A. Abdomen not much (if at all) wider or longer than cephalothorax;
    body firm, well armored, and well pigmented — the familiar fresh water crayfishes and, as the only local
    marine representative, the lobster Homarus americanus
    (ENTER 31)

    B. Abdomen markedly wider and/or longer than cephalothorax; bodies soft, seemingly weak; burrowing forms with
    a tendency to reduced eyes and pigmentation
    (ENTER 3)

  Animals of typically crablike form, with the fifth (last) pair of thoracic legs not markedly reduced;
  abdomen reduced, folded under body, lacking uropods;
  antennae short and set medial to the eyes — the 'true crabs'
  (ENTER 5)

  Fifth pair of thoracic legs very reduced and folded up above the bases of the fourth pair;
  body form varies from quite crablike to not very crablike;
  abdomen in some is asymmetrical, or twisted, or reduced, but with uropods
  (ENTER 27)"))

if (a=="1"){
  infraorder<-"Caridea"
  print("Your organism is in the Infraorder CARIDEA!")

  if ("choice_2.jpg" %in% image_fdir == TRUE) {
    image_path<- paste0(image.folder, "choice_2.jpg")
    choice2<-magick::image_read(image_path)
    op <- par(mfrow=c(1,2))
    plot(choice2)
    title(main=expression( bold("ENTER 2")), line = -4)

    if ("choice_32.jpg" %in% image_fdir == TRUE){
      image_path <- paste0(image.folder, "choice_32.jpg")
      choice32<-magick::image_read(image_path)
      plot(choice32)
      title(main=expression( bold("ENTER 32")), line = -4)
    } else if ("choice_32.jpg" %in% image_fdir == FALSE) {
      plot(0,0)
      title(main=expression( bold("ERROR")), line = -4)
    }

  }

  b <- readline(cat("Which applies to your organism?
  Carpus (the segment next to the claw or hand) of second pair of legs not annulated (fig. 40) (ENTER 2)

  Carpus of second pair of legs annulated into three parts (fig. 39);
  with humped abdomen (Family HIPPOLYTIDAE) (ENTER 32)"))
  if (is.null(dev.list()) == FALSE){
    dev.off()
  }
} else {
  if (a=="31"){
    infraorder<-"Astacidea"
    family<-"Nephropidae"
    genus_species<-"Homarus americanus"
    print("Your organism is in the Infraorder ASTACIDEA!")
    print(paste("Identification! The only local marine representative, the lobster Homarus americanus"))
  } else {
    if (a=="3"){
      infraorder<-"Axiidea"
      print("Your organism is in the Infraorder AXIIDEA!")

      if ("choice_35.jpg" %in% image_fdir == TRUE) {
        image_path <- paste0(image.folder, "choice_35.jpg")
        choice35<-magick::image_read(image_path)
        op <- par(mfrow=c(1,2))
        plot(choice35)
        title(main=expression( bold("ENTER 35")), line = -4)

        if ("choice_4.jpg" %in% image_fdir == TRUE){
          image_path <- paste0(image.folder, "choice_4.jpg")
          choice4<-magick::image_read(image_path)
          plot(choice4)
          title(main=expression( bold("ENTER 4")), line = -4)
        } else if ("choice_4.jpg" %in% image_fdir == FALSE) {
          plot(0,0)
          title(main=expression( bold("ERROR")), line = -4)
        }

      }

      b <- readline(cat("Which applies to your organism?

  Chelipeds unequal in size; body very pale (ENTER 35)

  Chelipeds of equal size (ENTER 4)"))
      if (is.null(dev.list()) == FALSE){
        dev.off()
      }
    } else {
      if (a=="5"){
        infraorder<-"Brachyura"
        print("You organism is in the Infraorder BRACHYURA!")

        if ("choice_6.jpg" %in% image_fdir == TRUE) {
          image_path <- paste0(image.folder, "choice_6.jpg")
          choice6<-magick::image_read(image_path)
          op <- par(mfrow=c(1,2))
          plot(choice6)
          title(main=expression( bold("ENTER 6")), line = -4)

          if ("choice_9.jpg" %in% image_fdir == TRUE){
            image_path <- paste0(image.folder, "choice_9.jpg")
            choice9<-magick::image_read(image_path)
            plot(choice9)
            title(main=expression( bold("ENTER 9")), line = -4)
          } else if ("choice_9.jpg" %in% image_fdir == FALSE) {
            plot(0,0)
            title(main=expression( bold("ERROR")), line = -4)
          }

        }

        b <- readline(cat("Which applies to your organism?

  Carapace triangular or globose, with apex projecting forward to form a rostrum; 'spider crabs' (ENTER 6)

  Carapace usually broader than long; rostrum small or wanting (ENTER 9)"))
        if (is.null(dev.list()) == FALSE){
          dev.off()
        }
      } else {
        if (a=="27"){
          infraorder<-"Anomura"
          print("You organism is in the Infraorder ANOMURA!")

          if ("choice_28.jpg" %in% image_fdir == TRUE) {

            image_path <- paste0(image.folder, "choice_28.jpg")
            choice28<-magick::image_read(image_path)
            op <- par(mfrow=c(1,2))
            plot(choice28)
            title(main=expression( bold("ENTER 28")), line = -4)

            if ("choice_30.jpg" %in% image_fdir == TRUE){
              image_path <- paste0(image.folder, "choice_30.jpg")
              choice30<-magick::image_read(image_path)
              plot(choice30)
              title(main=expression( bold("ENTER 30")), line = -4)
            } else if ("choice_30.jpg" %in% image_fdir == FALSE) {
              plot(0,0)
              title(main=expression( bold("ERROR")), line = -4)
            }
          }

          b <- readline(cat("Which applies to your organism?

  Animals inhabit gastropod shells; abdomen soft and twisted ('hermit crabs') (ENTER 28)

  Abdomen symmetrical and tucked under thorax (ENTER 30)"))

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
  }
}

if (b=="2") {

  if ("choice_33.jpg" %in% image_fdir == TRUE) {
    image_path <- paste0(image.folder, "choice_33.jpg")
    choice33<-magick::image_read(image_path)
    op <- par(mfrow=c(1,2))
    plot(choice33)
    title(main=expression( bold("ENTER 33")), line = -4)

    if ("choice_34.jpg" %in% image_fdir == TRUE){
      image_path <- paste0(image.folder, "choice_34.jpg")
      choice34<-magick::image_read(image_path)
      plot(choice34)
      title(main=expression( bold("ENTER 34")), line = -4)
    } else if ("choice_34.jpg" %in% image_fdir == FALSE) {
      plot(0,0)
      title(main=expression( bold("ERROR")), line = -4)
    }

  }

  c<-readline(cat("Which applies to your organism?

  Rostrum short, not compressed; first pair of legs subchelate (fig. 31);
  second pair of legs chelate (as in fig. 41); eyes set close together (Family CRANGONIDAE) (ENTER 33)

  Rostrum long and laterally compressed; first and second pair of legs chelate;
  eyes set widely apart (Family PALAEMONIDAE) (ENTER 34)"))
  if (is.null(dev.list()) == FALSE){
      dev.off()
    }
  } else {
  if (b=="32"){
        family<-"Hippolytidae"
        genus_species <- "Hippolyte zostericola"
        print("Your organism is in the HIPPOLYTIDAE family!")
        print ("Identification! Hippolyte zostericola")
      } else {
    if (b=="35"){
          family<-"Callianassidae"
          genus_species <- "Callianassa atlantica"
          print ("Identification! Callianassa atlantica")
        } else {
      if (b=="4"){
        if ("choice_36.jpg" %in% image_fdir == TRUE) {
          image_path <- paste0(image.folder, "choice_36.jpg")
          choice36<-magick::image_read(image_path)
          op <- par(mfrow=c(1,2))
          plot(choice36)
          title(main=expression( bold("ENTER 36")), line = -4)

          if ("choice_37.jpg" %in% image_fdir == TRUE){
            image_path <- paste0(image.folder, "choice_37.jpg")
            choice37<-magick::image_read(image_path)
            plot(choice37)
            title(main=expression( bold("ENTER 37")), line = -4)
          } else if ("choice_37.jpg" %in% image_fdir == FALSE) {
            plot(0,0)
            title(main=expression( bold("ERROR")), line = -4)
          }

        }

        c<-readline(cat("Which applies to your organism?

  Chelipeds with fingers not deflexed (fig. 30); fairly common and of good size (ca. 100 mm long) (ENTER 36)

  Chelipeds with fingers deflexed (subchelate, fig. 29); a rare animal of small size (to ca. 35 mm) (ENTER 37)"))
        if (is.null(dev.list()) == FALSE){
              dev.off()
            }
          } else {
        if (b=="6"){
          if ("choice_38.jpg" %in% image_fdir == TRUE) {
            image_path <- paste0(image.folder, "choice_38.jpg")
            choice38<-magick::image_read(image_path)
            op <- par(mfrow=c(1,2))
            plot(choice38)
            title(main=expression( bold("ENTER 38")), line = -4)

            if ("choice_34.jpg" %in% image_fdir == TRUE){
              image_path <- paste0(image.folder, "choice_34.jpg")
              choice34<-magick::image_read(image_path)
              plot(choice34)
              title(main=expression( bold("ENTER 34")), line = -4)
            } else if ("choice_34.jpg" %in% image_fdir == FALSE) {
              plot(0,0)
              title(main=expression( bold("ERROR")), line = -4)
            }

          }
          c<-readline(cat("Which applies to your organism?

  Carapace triangular, resembling a small chip of stone (fig. 3);
  walking legs very small but chelipeds relatively huge (Family PARTHENOPIDAE) (ENTER 38)

  Walking legs long and conspicuous (previously MAIIDAE) (Family EPIALTIDAE) (ENTER 7)"))
          if (is.null(dev.list()) == FALSE){
                dev.off()
              }
            } else {
          if (b=="9"){
            if ("choice_10.jpg" %in% image_fdir == TRUE) {
              image_path <- paste0(image.folder, "choice_10.jpg")
              choice10<-magick::image_read(image_path)
              op <- par(mfrow=c(1,2))
              plot(choice10)
              title(main=expression( bold("ENTER 10")), line = -4)

              if ("choice_19.jpg" %in% image_fdir == TRUE){
                image_path <- paste0(image.folder, "choice_19.jpg")
                choice19<-magick::image_read(image_path)
                plot(choice19)
                title(main=expression( bold("ENTER 19")), line = -4)
              } else if ("choice_19.jpg" %in% image_fdir == FALSE) {
                plot(0,0)
                title(main=expression( bold("ERROR")), line = -4)
              }

            }
            c<-readline(cat("Which applies to your organism?

  Free living crabs, well pigmented, with eyes not reduced (ENTER 10)

  Small crabs with reduced eyes; commonly but not always with reduced body pigmentation;
  usually commensal in worm tubes or in bivalves, but some may wander free;
  carapaces either subcircular or markedly widened from side to side;
  small, rarely reaching 2.5 cm across carapace (ENTER 19)"))
            if (is.null(dev.list()) == FALSE){
              dev.off()
            }
              } else {
            if (b=="28"){
              family<-"Paguridae"
              print("Your organism is in the Paguridae family!")
              if ("choice_29.jpg" %in% image_fdir == TRUE) {
                image_path <- paste0(image.folder, "choice_29.jpg")
                choice29<-magick::image_read(image_path)
                op <- par(mfrow=c(1,2))
                plot(choice29)
                title(main=expression( bold("ENTER 29")), line = -4)

                if ("choice_56.jpg" %in% image_fdir == TRUE){
                  image_path <- paste0(image.folder, "choice_56.jpg")
                  choice56<-magick::image_read(image_path)
                  plot(choice56)
                  title(main=expression( bold("ENTER 56")), line = -4)
                } else if ("choice_56.jpg" %in% image_fdir == FALSE) {
                  plot(0,0)
                  title(main=expression( bold("ERROR")), line = -4)
                }

              }
              c<-readline(cat("Which applies to your organism?

  Chelipeds slender; hands subcylindrical (ENTER 29)

  Hands broad, flat, tuberculate (ENTER 56)"))
              if (is.null(dev.list()) == FALSE){
                dev.off()
              }

                } else {
              if (b=="30"){
                if ("choice_42.jpg" %in% image_fdir == TRUE) {
                  image_path <- paste0(image.folder, "choice_42.jpg")
                  choice42<-magick::image_read(image_path)
                  op <- par(mfrow=c(1,2))
                  plot(choice42)
                  title(main=expression( bold("ENTER 42")), line = -4)

                  if ("choice_43.jpg" %in% image_fdir == TRUE){
                    image_path <- paste0(image.folder, "choice_43.jpg")
                    choice43<-magick::image_read(image_path)
                    plot(choice43)
                    title(main=expression( bold("ENTER 43")), line = -4)
                  } else if ("choice_43.jpg" %in% image_fdir == FALSE) {
                    plot(0,0)
                    title(main=expression( bold("ERROR")), line = -4)
                  }

                }
                c<-readline(cat("Which applies to your organism?

  Form essentially crablike, except for having fifth (last) thoracic legs reduced and hidden under carapace
  (fig. 11); small, rounded;
  chelipeds directed forward in resting position (not transversely as in Brachyura);
  antennae long and set lateral to eyes (Family PORCELLANIDAE) (ENTER 42)

  Body egg shaped; carapace gray and shining; telson forms a long triangular white ventral shield;
  burrows in waveswept sandy beaches (Family HIPPIDAE; 'mole crabs', 'sand bugs') (ENTER 43)"))

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
      }
    }
  }
}

if (c=="33"){
    family<-"Crangonidae"
    genus_species <- "Crangon septemspinosus"
    print("Your organism is in the Crangonidae family!")
    print ("Identification! Crangon septemspinosus")
  } else {
  if (c=="34"){
      family<-"Palaemonidae"
      genus_species <- "Palaemonetes"
      print("Your organism is in the Palaemonidae family!")
      print ("Identification! Palaemonetes")
    } else {
    if (c=="36"){
        family<-"Upogebiidae"
        genus_species <- "Upogebia affinis"
        print("Your organism is in the Upogebiidae family!")
        print ("Identification! Upogebia affinis")
      } else {
      if (c=="37"){
          family<-"Laomediidae"
          genus_species <- "Naushonia crangonoides"
          print("Your organism is in the Laomediidae family!")
          print ("Identification! Naushonia crangonoides")
        } else {
        if (c=="38"){
            family<-"Parthenopidae"
            genus_species <- "Heterocrypta granulata"
            print("Your organism is in the Parthenopidae family!")
            print ("Identification! Heterocrypta granulata")
          } else {
          if (c=="7"){
              family<-"EPIALTIDAE"
              print("Your organism is in the EPIALTIDAE family!")

              if ("choice_8.jpg" %in% image_fdir == TRUE) {
                image_path <- paste0(image.folder, "choice_8.jpg")
                choice8<-magick::image_read(image_path)
                op <- par(mfrow=c(1,2))
                plot(choice8)
                title(main=expression( bold("ENTER 8")), line = -4)

                if ("choice_39.jpg" %in% image_fdir == TRUE){
                  image_path <- paste0(image.folder, "choice_39.jpg")
                  choice39<-magick::image_read(image_path)
                  plot(choice39)
                  title(main=expression( bold("ENTER 39")), line = -4)
                } else if ("choice_39.jpg" %in% image_fdir == FALSE) {
                  plot(0,0)
                  title(main=expression( bold("ERROR")), line = -4)
                }

              }

              d<-readline(cat("Which applies to your organism?

  Carapace with medial dorsal spines (ENTER 8)

  Carapace surface smooth, without median dorsal spines; splotched with red (ENTER 39)"))
              if (is.null(dev.list()) == FALSE){
                dev.off()
              }
            } else {
            if (c=="10") {
              if ("choice_11.jpg" %in% image_fdir == TRUE) {
                image_path <- paste0(image.folder, "choice_11.jpg")
                choice11<-magick::image_read(image_path)
                op <- par(mfrow=c(1,2))
                plot(choice11)
                title(main=expression( bold("ENTER 11")), line = -4)

                if ("choice_23.jpg" %in% image_fdir == TRUE){
                  image_path <- paste0(image.folder, "choice_23.jpg")
                  choice23<-magick::image_read(image_path)
                  plot(choice23)
                  title(main=expression( bold("ENTER 23")), line = -4)
                } else if ("choice_23.jpg" %in% image_fdir == FALSE) {
                  plot(0,0)
                  title(main=expression( bold("ERROR")), line = -4)
                }

              }
                d<-readline(cat("Which applies to your organism?

  Carapace outline varies, but characteristically front margin is curved and bears a series of teeth between the
  eye and the (antero) lateral corner on each side; size varies, but several species over 5 cm across carapace
  belong here —'cancroid' crabs (ENTER 11)

  Carapace outline squarish, with a more or less straight front margin;
  most are active, semi-terrestrial crabs, rarely over 4 cm across carapace;
  the free living 'grapsoid' crabs (ENTER 23)"))
                if (is.null(dev.list()) == FALSE){
                  dev.off()
                }
              } else {
              if (c=="19") {
                if ("choice_30.jpg" %in% image_fdir == TRUE) {
                  image_path <- paste0(image.folder, "choice_30.jpg")
                  choice30<-magick::image_read(image_path)
                  op <- par(mfrow=c(1,2))
                  plot(choice30)
                  title(main=expression( bold("ENTER 30")), line = -4)

                  if ("choice_20.jpg" %in% image_fdir == TRUE){
                    image_path <- paste0(image.folder, "choice_20.jpg")
                    choice20<-magick::image_read(image_path)
                    plot(choice20)
                    title(main=expression( bold("ENTER 20")), line = -4)
                  } else if ("choice_20.jpg" %in% image_fdir == FALSE) {
                    plot(0,0)
                    title(main=expression( bold("ERROR")), line = -4)
                  }

                }
                  d<-readline(cat("Which applies to your organism?

  Fifth (last) pair of thoracic legs very small and tucked up under carapace.
  You are in the wrong part of key; animal is a brachyurous anomuran. (ENTER 30)

  Fifth (last) pair of thoracic legs is not especially reduced, useful in locomotion.
  This is a difficult family with marked sexual dimorphism (Family PINNOTHERIDAE) (ENTER 20)"))
                  if (is.null(dev.list()) == FALSE){
                    dev.off()
                  }
                } else {
                if (c=="29"){

                  if ("choice_57.jpg" %in% image_fdir == TRUE) {
                    image_path <- paste0(image.folder, "choice_57.jpg")
                    choice57<-magick::image_read(image_path)
                    op <- par(mfrow=c(1,2))
                    plot(choice57)
                    title(main=expression( bold("ENTER 57")), line = -4)

                    if ("choice_58.jpg" %in% image_fdir == TRUE){
                      image_path <- paste0(image.folder, "choice_58.jpg")
                      choice58<-magick::image_read(image_path)
                      plot(choice58)
                      title(main=expression( bold("ENTER 58")), line = -4)
                    } else if ("choice_58.jpg" %in% image_fdir == FALSE) {
                      plot(0,0)
                      title(main=expression( bold("ERROR")), line = -4)
                    }

                  }

                  d<-readline(cat("Which applies to your organism?

  Chelae with four or five distinct purplish bands on whitish background (ENTER 57)

  Chelae without such distinct banding (ENTER 58)"))
                  if (is.null(dev.list()) == FALSE){
                    dev.off()
                  }
                } else {
                  if (c=="56"){
                    genus_species <- "Pagurus pollicaris"
                    print ("Identification! Pagurus pollicaris")
                    } else {
                    if (c=="42"){
                    family<-"PORCELLANIDAE"
                    print("Your organism is in the PORCELLANIDAE family!")
                    genus_species <- "Polyonyx macrocheles"
                    print ("Identification! Polyonyx macrocheles")
                } else {
                      if (c=="43"){
                    family<-"HIPPIDAE"
                    print("Your organism is in the HIPPIDAE family!")
                    genus_species <- "Emerita talpoida"
                    print ("Identification! Emerita talpoida")
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
        }
      }
    }
  }
}

if (d=="8"){
  if ("choice_40.jpg" %in% image_fdir == TRUE) {
    image_path <- paste0(image.folder, "choice_40.jpg")
    choice40<-magick::image_read(image_path)
    op <- par(mfrow=c(1,2))
    plot(choice40)
    title(main=expression( bold("ENTER 40")), line = -4)

    if ("choice_41.jpg" %in% image_fdir == TRUE){
      image_path <- paste0(image.folder, "choice_41.jpg")
      choice41<-magick::image_read(image_path)
      plot(choice41)
      title(main=expression( bold("ENTER 41")), line = -4)
    } else if ("choice_41.jpg" %in% image_fdir == FALSE) {
      plot(0,0)
      title(main=expression( bold("ERROR")), line = -4)
    }

  }
  e<-readline(cat("Which applies to your organism?

  With six spines in the median dorsal row; few dorsal tubercles (fig. 1) (ENTER 40)

  With nine spines in median dorsal row; many dorsal tubercles (fig. 2) (ENTER 41)"))
  if (is.null(dev.list()) == FALSE){
    dev.off()
  }
} else {
  if (d=="39"){
    genus_species <- "Pelia mutica"
    print ("Identification! Pelia mutica")
  } else {
    if (d=="11"){

      if ("choice_12.jpg" %in% image_fdir == TRUE) {
        image_path <- paste0(image.folder, "choice_12.jpg")
        choice12<-magick::image_read(image_path)
        op <- par(mfrow=c(1,2))
        plot(choice12)
        title(main=expression( bold("ENTER 12")), line = -4)

        if ("choice_13.jpg" %in% image_fdir == TRUE){
          image_path <- paste0(image.folder, "choice_13.jpg")
          choice13<-magick::image_read(image_path)
          plot(choice13)
          title(main=expression( bold("ENTER 13")), line = -4)
        } else if ("choice_13.jpg" %in% image_fdir == FALSE) {
          plot(0,0)
          title(main=expression( bold("ERROR")), line = -4)
        }

      }
      e<-readline(cat("Which applies to your organism?

  First antennae (antennules) folded longitudinally or nearly so (fig. 4) (Family CANCRIDAE) (ENTER 12)

  First antennae folded transversely or obliquely (fig. 5) (ENTER 13)"))
      if (is.null(dev.list()) == FALSE){
        dev.off()
      }
    } else {
      if (d=="23"){

        if ("choice_24.jpg" %in% image_fdir == TRUE) {
          image_path <- paste0(image.folder, "choice_24.jpg")
          choice24<-magick::image_read(image_path)
          op <- par(mfrow=c(1,2))
          plot(choice24)
          title(main=expression( bold("ENTER 24")), line = -4)

          if ("choice_25.jpg" %in% image_fdir == TRUE){
            image_path <- paste0(image.folder, "choice_25.jpg")
            choice25<-magick::image_read(image_path)
            plot(choice25)
            title(main=expression( bold("ENTER 25")), line = -4)
          } else if ("choice_25.jpg" %in% image_fdir == FALSE) {
            plot(0,0)
            title(main=expression( bold("ERROR")), line = -4)
          }

        }

        e<-readline(cat("Which applies to your organism?

  A gap is left between the third maxillipeds when they are held at rest;
  front very or moderately broad; eyestalks short (Family GRAPSIDAE) (ENTER 24)

  The third maxillipeds almost or quite close over the mouth region when held at rest;
  front moderately or very narrow; eyestalks long; one cheliped of male very large;
  local representatives are the typical 'fiddler crabs' (Family OCYPODIDAE) (ENTER 25)"))
        if (is.null(dev.list()) == FALSE){
          dev.off()
        }
      } else {
        if (d=="30"){
          infraorder<-"Anomura"

          if ("choice_42.jpg" %in% image_fdir == TRUE) {
            image_path <- paste0(image.folder, "choice_42.jpg")
            choice42<-magick::image_read(image_path)
            op <- par(mfrow=c(1,2))
            plot(choice42)
            title(main=expression( bold("ENTER 42")), line = -4)

            if ("choice_43.jpg" %in% image_fdir == TRUE){
              image_path <- paste0(image.folder, "choice_43.jpg")
              choice43<-magick::image_read(image_path)
              plot(choice43)
              title(main=expression( bold("ENTER 43")), line = -4)
            } else if ("choice_43.jpg" %in% image_fdir == FALSE) {
              plot(0,0)
              title(main=expression( bold("ERROR")), line = -4)
            }

          }
          e<-readline(cat("Which applies to your organism?

  Form essentially crablike, except for having fifth (last) thoracic legs reduced and hidden under carapace
  (fig. 11); small, rounded;
  chelipeds directed forward in resting position (not transversely as in Brachyura);
  antennae long and set lateral to eyes (Family PORCELLANIDAE) (ENTER 42)

  Body egg shaped; carapace gray and shining; telson forms a long triangular white ventral shield;
  burrows in waveswept sandy beaches (Family HIPPIDAE; 'mole crabs', 'sand bugs') (ENTER 43)"))
          if (is.null(dev.list()) == FALSE){
            dev.off()
          }
        } else {
          if (d=="57"){
            genus_species <- "Pagurus annulipes"
            print ("Identification! Pagurus annulipes")
          } else {
            if (d=="58"){
              genus_species <- "Pagurus longicarpus"
              print ("Identification! Pagurus longicarpus")
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

if (e=="40"){
  genus_species <- "Libinia dubia"
  print ("Identification! Libinia dubia")
} else {
  if (e=="41"){
    genus_species <- "Libinia emarginata"
    print ("Identification! Libinia emarginata")
  } else {
    if (e=="12"){
      family<-"CANCRIDAE"
      print("Your organism is in the CANCRIDAE family!")

      if ("choice_44.jpg" %in% image_fdir == TRUE) {
        image_path <- paste0(image.folder, "choice_44.jpg")
        choice44<-magick::image_read(image_path)
        op <- par(mfrow=c(1,2))
        plot(choice44)
        title(main=expression( bold("ENTER 44")), line = -4)

        if ("choice_45.jpg" %in% image_fdir == TRUE){
          image_path <- paste0(image.folder, "choice_45.jpg")
          choice45<-magick::image_read(image_path)
          plot(choice45)
          title(main=expression( bold("ENTER 45")), line = -4)
        } else if ("choice_45.jpg" %in% image_fdir == FALSE) {
          plot(0,0)
          title(main=expression( bold("ERROR")), line = -4)
        }

      }

      f<-readline(cat("Which applies to your organism?

  Edges of antero-lateral teeth entire; chelipeds granulate, not denticulate (fig. 6) (ENTER 44)

  Edges of antero-lateral teeth denticulate; upper margin of palm denticulate (fig. 7) (ENTER 45)"))

      if (is.null(dev.list()) == FALSE){
        dev.off()
      }

    } else{
      if (e=="13"){
        if ("choice_14.jpg" %in% image_fdir == TRUE) {
          image_path <- paste0(image.folder, "choice_14.jpg")
          choice14<-magick::image_read(image_path)
          op <- par(mfrow=c(1,2))
          plot(choice14)
          title(main=expression( bold("ENTER 14")), line = -4)

          if ("choice_18.jpg" %in% image_fdir == TRUE){
            image_path <- paste0(image.folder, "choice_18.jpg")
            choice18<-magick::image_read(image_path)
            plot(choice18)
            title(main=expression( bold("ENTER 18")), line = -4)
          } else if ("choice_18.jpg" %in% image_fdir == FALSE) {
            plot(0,0)
            title(main=expression( bold("ERROR")), line = -4)
          }

        }

        f<-readline(cat("Which applies to your organism?

  Last pair of walking legs not markedly adapted for swimming, tips sharp, for walking (fig. 10) (ENTER 14)

  Last pair of legs flattened and paddle-like; tips rounded, for swimming (figs. 8, 9) (ENTER 18)"))

        if (is.null(dev.list()) == FALSE){
          dev.off()
        }
      } else{
        if (e=="24"){
          family<-"GRAPSIDAE"
          print("Your organism is in the GRAPSIDAE family!")

          if ("choice_46.jpg" %in% image_fdir == TRUE) {
            image_path <- paste0(image.folder, "choice_46.jpg")
            choice46<-magick::image_read(image_path)
            op <- par(mfrow=c(1,2))
            plot(choice46)
            title(main=expression( bold("ENTER 46")), line = -4)

            if ("choice_47.jpg" %in% image_fdir == TRUE){
              image_path <- paste0(image.folder, "choice_47.jpg")
              choice47<-magick::image_read(image_path)
              plot(choice47)
              title(main=expression( bold("ENTER 47")), line = -4)
            } else if ("choice_47.jpg" %in% image_fdir == FALSE) {
              plot(0,0)
              title(main=expression( bold("ERROR")), line = -4)
            }

          }

          f<-readline(cat("Which applies to your organism?

 Carapace very square (fig. 19); dark plum colored to bluish black (the 'square backed fiddler');
 found burrowing in salt marshes (ENTER 46)

 Carapace with corners rounded off (fig. 20); color variably blotched olive-green;
 drifts in on Sargassum ('Columbus crab', 'Gulf-weed crab') (ENTER 47)"))

          if (is.null(dev.list()) == FALSE){
            dev.off()
          }
        } else{
          if (e=="25"){
            family<-"OCYPODIDAE"
            print("Your organism is in the OCYPODIDAE family!")

            if ("choice_26.jpg" %in% image_fdir == TRUE) {
              image_path <- paste0(image.folder, "choice_26.jpg")
              choice26<-magick::image_read(image_path)
              op <- par(mfrow=c(1,2))
              plot(choice26)
              title(main=expression( bold("ENTER 26")), line = -4)

              if ("choice_48.jpg" %in% image_fdir == TRUE){
                image_path <- paste0(image.folder, "choice_48.jpg")
                choice48<-magick::image_read(image_path)
                plot(choice48)
                title(main=expression( bold("ENTER 48")), line = -4)
              } else if ("choice_48.jpg" %in% image_fdir == FALSE) {
                plot(0,0)
                title(main=expression( bold("ERROR")), line = -4)
              }

            }

            f<-readline(cat("Which applies to your organism?

  Inner surface of large claw of male with an oblique ridge (fig. 22) (ENTER 26)

  Inner surface of large claw of male without such oblique ridge (fig. 21);
  carapace mottled in grays and purple ('calico back') (ENTER 48)"))

            if (is.null(dev.list()) == FALSE){
              dev.off()
            }
          } else {
            if (e=="42"){
              family<-"PORCELLANIDAE"
              print("Your organism is in the PORCELLANIDAE family!")
              genus_species <- "Polyonyx macrocheles"
              print ("Identification! Polyonyx macrocheles")
            } else {
              if (e=="43"){
                family<-"HIPPIDAE"
                print("Your organism is in the HIPPIDAE family!")
                genus_species <- "Emerita talpoida"
                print ("Identification! Emerita talpoida")
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

if (f=="44"){
  genus_species <- "Cancer irroratus"
  print ("Identification! Cancer irroratus")
} else {
  if (f=="45"){
    genus_species <- "Cancer borealis"
    print ("Identification! Cancer borealis")
  } else {
    if (f=="14"){

      if ("choice_49.jpg" %in% image_fdir == TRUE) {
        image_path <- paste0(image.folder, "choice_49.jpg")
        choice49<-magick::image_read(image_path)
        op <- par(mfrow=c(1,2))
        plot(choice49)
        title(main=expression( bold("ENTER 49")), line = -4)

        if ("choice_15.jpg" %in% image_fdir == TRUE){
          image_path <- paste0(image.folder, "choice_15.jpg")
          choice15<-magick::image_read(image_path)
          plot(choice15)
          title(main=expression( bold("ENTER 15")), line = -4)
        } else if ("choice_15.jpg" %in% image_fdir == FALSE) {
          plot(0,0)
          title(main=expression( bold("ERROR")), line = -4)
        }

      }

      g<-readline(cat("Which applies to your organism?

  Front (region between eyes) produced into three low teeth; five very prominent sharp anterolateral teeth (fig. 10);
  last pair of legs slightly flattened;
  an active long-legged crab commonly 5-7.5 cm across carapace — the common 'green crab' (but color varies from greenish-black to orange) (ENTER 49)

  Front not produced into teeth;
  generally small crabs, but more heavily built and less active than Carcinus, with shorter legs and heavier chelae;
  tips of chelae dark in some species; a group in which specific identification requires care (Family XANTHIDAE) (ENTER 15)"))
      if (is.null(dev.list()) == FALSE){
        dev.off()
      }

    } else {
      if (f=="18"){

        if ("choice_50.jpg" %in% image_fdir == TRUE) {
          image_path <- paste0(image.folder, "choice_50.jpg")
          choice50<-magick::image_read(image_path)
          op <- par(mfrow=c(1,2))
          plot(choice50)
          title(main=expression( bold("ENTER 50")), line = -4)

          if ("choice_51.jpg" %in% image_fdir == TRUE){
            image_path <- paste0(image.folder, "choice_51.jpg")
            choice51<-magick::image_read(image_path)
            plot(choice51)
            title(main=expression( bold("ENTER 51")), line = -4)
          } else if ("choice_51.jpg" %in% image_fdir == FALSE) {
            plot(0,0)
            title(main=expression( bold("ERROR")), line = -4)
          }

        }

        g<-readline(cat("Which applies to your organism?

  Antero-lateral teeth on carapace three to five in number (fig. 9);
  carapace not usually broad; color cream to tan with red markings ('lady crab') (ENTER 50)

  Antero-lateral teeth nine in number (fig. 8); carapace extremely broad;
  outermost lateral tooth especially long and sharp;
  with some blue coloration, particularly on chelipeds ('blue crab') (ENTER 51)"))

        if (is.null(dev.list()) == FALSE){
          dev.off()
        }

      } else {
        if (f=="46"){
          genus_species <- "Sesarma reticulatum"
          print ("Identification! Sesarma reticulatum")
        } else {
          if (f=="47"){
            genus_species <- "Planes minutus"
            print ("Identification! Planes minutus")
          } else {
            if (f=="26"){
              if ("choice_52.jpg" %in% image_fdir == TRUE) {
                image_path <- paste0(image.folder, "choice_52.jpg")
                choice52<-magick::image_read(image_path)
                op <- par(mfrow=c(1,2))
                plot(choice52)
                title(main=expression( bold("ENTER 52")), line = -4)

                if ("choice_53.jpg" %in% image_fdir == TRUE){
                  image_path <- paste0(image.folder, "choice_53.jpg")
                  choice53<-magick::image_read(image_path)
                  plot(choice53)
                  title(main=expression( bold("ENTER 53")), line = -4)
                } else if ("choice_53.jpg" %in% image_fdir == FALSE) {
                  plot(0,0)
                  title(main=expression( bold("ERROR")), line = -4)
                }

              }
              g<-readline(cat("Which applies to your organism?

  Carapace uniformly dark, almost black;
  as a rule not exceeding 22 mm in width; very numerous (the 'black fiddler crab') (ENTER 52)

  Carapace grayish, lighter at front, with central brown mark;
  size very much larger than in the common fiddler crabs;
  carapace commonly over 23 mm, even 36 mm wide;
  reddish spots at joints of chelipeds, especially in males ('big fiddler crab', 'red-jointed fiddler crab')
  (ENTER 53)"))

              if (is.null(dev.list()) == FALSE){
                dev.off()
              }

            } else {
              if (f=="48"){
                genus_species <- "Uca pugilator"
                print ("Identification! Uca pugilator")
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

if (g=="49"){
  family<-"Carcinidae"
  print("Your organism is in the Carcinidae family!")
  genus_species <- "Carcinus maenas"
  print ("Identification! Carcinus maenas")
} else {
  if (g=="15"){
    family<-"XANTHIDAE"
    print("Your organism is in the XANTHIDAE family!")

    if ("choice_16.jpg" %in% image_fdir == TRUE) {
      image_path <- paste0(image.folder, "choice_16.jpg")
      choice16<-magick::image_read(image_path)
      op <- par(mfrow=c(1,2))
      plot(choice16)
      title(main=expression( bold("ENTER 16")), line = -4)

      if ("choice_54.jpg" %in% image_fdir == TRUE){
        image_path <- paste0(image.folder, "choice_54.jpg")
        choice54<-magick::image_read(image_path)
        plot(choice54)
        title(main=expression( bold("ENTER 54")), line = -4)
      } else if ("choice_54.jpg" %in% image_fdir == FALSE) {
        plot(0,0)
        title(main=expression( bold("ERROR")), line = -4)
      }

    }

    h<-readline(cat("Which applies to your organism?

  Frontal margin (between eyes) with a single edge, not seeming double; fingers of chelae dark (ENTER 16)

  Frontal margin transversely grooved so as to appear double; fingers of chelae whitish;
  carapace with four and three distinct transverse lines of granules on anterior half (ENTER 54)"))
    if (is.null(dev.list()) == FALSE){
      dev.off()
    }
  } else {
    if (g == "50"){
      family<-"Ovalipidae"
      print("Your organism is in the Ovalipidae family!")
      genus_species <- "Ovalipes ocellatus"
      print ("Identification! Ovalipes ocellatus")
    } else {
      if (g =="51"){
        family<-"Portunidae"
        print("Your organism is in the Portunidae family!")
        genus_species <- "Callinectes sapidus"
        print ("Identification! Callinectes sapidus")
      } else {
        if (g=="52"){
          genus_species <- "Uca pugnax"
          print ("Identification! Uca pugnax")
        } else {
          if (g=="53"){
            genus_species <- "Uca minax"
            print ("Identification! Uca minax")
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

if (h=="16"){
  if ("choice_55.jpg" %in% image_fdir == TRUE) {
    image_path <- paste0(image.folder, "choice_55.jpg")
    choice55<-magick::image_read(image_path)
    op <- par(mfrow=c(1,2))
    plot(choice55)
    title(main=expression( bold("ENTER 55")), line = -4)

    if ("choice_17.jpg" %in% image_fdir == TRUE){
      image_path <- paste0(image.folder, "choice_17.jpg")
      choice17<-magick::image_read(image_path)
      plot(choice17)
      title(main=expression( bold("ENTER 17")), line = -4)
    } else if ("choice_17.jpg" %in% image_fdir == FALSE) {
      plot(0,0)
      title(main=expression( bold("ERROR")), line = -4)
    }

  }
  i<-readline(cat("Which applies to your organism?

  Movable finger of major cheliped with a heavy blunt tooth near base (fig. 26);
  carapace width often exceeds 2.5 cm (fig. 25) (ENTER 55)

  Movable finger of major cheliped without such a tooth (ENTER 17)"))

  if (is.null(dev.list()) == FALSE){
    dev.off()
  }

} else {
  if (h=="54"){
    genus_species <- "Rhithropanopeus harrisii"
    print ("Identification! Rhithropanopeus harrisii")
  } else {
    if (h=="NA"){

    } else {
      print("ERROR! Not a choice!")
    }
  }
}


if (i=="55"){
  genus_species <- "Panopeus herbsti"
  print ("Identification! Panopeus herbsti")
} else {
  if (i=="17"){
    family <- "Panopeidae"
    print("Your organism is in the Panopeidae family!")
    if ("choice_59.jpg" %in% image_fdir == TRUE) {
      image_path <- paste0(image.folder, "choice_59.jpg")
      choice59<-magick::image_read(image_path)
      op <- par(mfrow=c(1,2))
      plot(choice59)
      title(main=expression( bold("ENTER 59")), line = -4)

      if ("choice_60.jpg" %in% image_fdir == TRUE){
        image_path <- paste0(image.folder, "choice_60.jpg")
        choice60<-magick::image_read(image_path)
        plot(choice60)
        title(main=expression( bold("ENTER 60")), line = -4)
      } else if ("choice_60.jpg" %in% image_fdir == FALSE) {
        plot(0,0)
        title(main=expression( bold("ERROR")), line = -4)
      }

    }
    j<-readline(cat("Which applies to your organism?

  With an elongated dark red spot on the inner (concealed) surface of each maxilliped
  (Note: Panopeus herbsti sometimes but not always has a comparable spot, but it is small, circular, and located
  near the base of the maxilliped).
  Fingers of minor chela 'spooned' (fig. 28);
  carapace rather flattened and oval in outline (fig. 27); uncommon (ENTER 59)

  Without such red spot on maxillipeds; fingers of minor chela not 'spooned' (Fig. 24);
  carapace outline more angular (fig. 23), not oval; very common (ENTER 60)"))

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

if (j=="59"){
  genus_species <- "Eurypanopeus depressus"
  print ("Identification! Eurypanopeus depressus")
} else {
  if (j=="60"){
    genus_species <- "Neopanope texana"
    print ("Identification! Neopanope texana")
  } else {
    if (j=="NA"){

    } else {
      print("ERROR! Not a choice!")
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
              Infraorder:", infraorder,
              "
              Family:", family,
              "
              Genus/Species:", genus_species,
"
"))

return("No Errors? Congratulations! You are clearly in your dichotomous key era! Slay!! If you have errors, no worries girlie pop, it can still be a brat summer! Just run that shit back!")
}
