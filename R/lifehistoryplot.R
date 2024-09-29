# Copyright 2024 Fisheries Queensland

# This file is part of SSAND.
# SSAND is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# SSAND is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with SSAND. If not, see <https://www.gnu.org/licenses/>.

#' Life history plot
#'
#' @param ss_mle A stock synthesis model output produced by `r4ss::SS_output()`
#' @param Linf Asymptotic length from VB growth model
#' @param k growth completion rate from VB growth model
#' @param a0 Age-at-length-zero from VB growth model
#' @param M Natural mortality as a single estimate.
#' @param L50 Length-at-50%- mature in cm.
#' @param L95 Length-at-95%- mature in cm.
#' @param WLa Weight-Length parameter A. Should be from W-L relationships expressed in kg and cm (as per Stock Synthesis).
#' @param WLb Weight-Length parameter B. Should be from W-L relationships expressed in kg and cm (as per Stock Synthesis).
#' @param MLS Minimum Legal Size in cm.
#' @param max_length Used to set an axis limits for lengths when Linf is not available.
#' @param scenario Specify scenario to use if inputting a list of scenarios. Default is 1.
#' @param nbreaks Approximate number of breaks along x axis.
#' @param colours Vectors of four colours to use: main lines, MLS rectangles, points , dotted reference lines
#' @param line_width Width of main lines
#' @param point_size Size of reference points
#' @param label_spacing_VB Spacing between labels for length vs age plot. Default is 0.07.
#' @param label_spacing_WL Spacing between labels for weight vs length plot. Default is 0.1.
#' @param label_spacing_M Spacing between labels for mortalityplot. Default is 0.07.
#' @param label_spacing_mat Spacing between labels for maturity plot. Default is 0.07.
#' @param sex Used for SS models to extract a single sex. Has a default of 1 for females. Use 2 for males
#' @param show_MLS_label Set to TRUE to show "Legal-sized" label.
#'
#' @return A cowplot object with four panels displaying species biology
#' @export
#' @examples
#' lifehistoryplot(ss_mle)
#' lifehistoryplot(Linf = 38, k = 0.3, a0 = 0,
#'                 M = 0.73, L50 = 18, L95 = 22,
#'                 WLa = 0.0013, WLb = 1.949,
#'                 MLS = 20, max_length = 45)
lifehistoryplot <- function(ss_mle = NULL,
                            Linf = NULL,
                            k = NULL,
                            a0 = NULL,
                            M = NULL,
                            L50 = NULL,
                            L95 = NULL,
                            WLa = NULL,
                            WLb = NULL,
                            MLS = NULL,
                            max_length = NULL,
                            sex = 1,
                            scenario = 1,
                            nbreaks = 10,
                            colours = c("grey70","grey70","black","grey30"),
                            line_width= 1,
                            point_size = 2,
                            label_spacing_VB = 0.07,
                            label_spacing_WL= 0.1,
                            label_spacing_M = 0.07,
                            label_spacing_mat = 0.07,
                            show_MLS_label = TRUE){

  if (missing(nbreaks)) {
    breaks <- scales::pretty_breaks()
  } else {
    breaks <- scales::pretty_breaks(nbreaks)
  }


  # Get SS pars if an ss_mle is provided
  if(!is.null(ss_mle)){

    if (check_scenarios(ss_mle,"SS","MLE")=="list of scenarios"){ss_mle <- ss_mle[[scenario]]}


    if( !any(sapply(list(Linf,k,a0,M,L50,L95,WLa,WLb), is.null)))
      warning("Parameter values have been supplied along with an ss_mle object.\nOnly the ss_mle results will be plotted.")

    if(is.null(sex)){
      message("SS sex not specified. Defaulting to '1'")
      sex = 1
    }

    SS_Biology <- ss_mle$Growth_Parameters |>
      dplyr::filter(Sex == sex) |>
      dplyr::mutate(L95 = Mat1 + 12)

    Linf = round(SS_Biology$Linf)
    k = SS_Biology$K
    a0 = SS_Biology$A_a_L0

    if(SS_Biology$M_age0 != SS_Biology$M_nages) {
      warning("SS did not use a single M estimate, therefore this parameter cannot be plotted")
    } else{
      M = SS_Biology$M_age0
    }

    if(SS_Biology$Mat1 == 0){
      L50 = NULL
      L95 = NULL
    } else{
      L50 = SS_Biology$Mat1

      L95 = data.frame(Length = 0:SS_Biology$Linf) |>
        dplyr::mutate(Mat = 1/(1+exp(SS_Biology$Mat2*(Length-SS_Biology$Mat1)))) |>
        dplyr::filter(Mat > 0.95) |>
        dplyr::slice(1) |>
        dplyr::pull(Length)

    }
    WLa = SS_Biology$WtLen1
    WLb = SS_Biology$WtLen2


  }

  # Growth curve plot ----
  if(!any(sapply(list(Linf, k, a0), is.null))){

    longevity <- ceiling((log(1-((Linf*0.99)/Linf))/-k)+a0)

    growth_curve <- data.frame(Age = 0:longevity) |>
      dplyr::mutate(Length = Linf*(1-exp(-k*(Age-a0)))
      )

    growth_plot <- ggplot2::ggplot(growth_curve, ggplot2::aes(Age, Length))+
      ggplot2::ggtitle("von Bertalanffy growth curve")+
      ggplot2::geom_line(col = colours[1], linewidth = line_width)+
      ggplot2::scale_y_continuous(breaks = breaks, name = "Length (cm)", expand = c(0,0),
                                  limits = c(0, max(growth_curve$Length)*1.2))+
      ggplot2::scale_x_continuous(breaks = breaks, name = "Age (years)", expand = c(0,0))+
      ggplot2::theme_bw()

    if(!is.null(MLS))
      growth_plot <- growth_plot +
      ggplot2::annotate("rect",ymin = MLS, ymax = Inf, xmin = -Inf,xmax = Inf, alpha = 0.2, fill = colours[2], linetype = "dashed", col = "black")#+


    if(!is.null(M)){
      Max_age_M <-ceiling(5.4/M)

      growth_plot <- growth_plot +
        ggplot2::annotate(geom = "segment", x = 0, xend = Max_age_M, y = Linf*(1-exp(-k*(Max_age_M-a0))), yend = Linf*(1-exp(-k*(Max_age_M-a0))), linetype = "dotted", color = colours[4]) +
        ggplot2::annotate(geom = "segment", x = Max_age_M, xend = Max_age_M, y = 0, yend = Linf*(1-exp(-k*(Max_age_M-a0))), linetype = "dotted", color = colours[4]) +
        ggplot2::annotate(geom="point", x=Max_age_M, y=Linf*(1-exp(-k*(Max_age_M-a0))), shape = 15, size = point_size, col = colours[3]) +
        ggplot2::annotate("text", x = Max_age_M-2, y = Linf*(1-exp(-k*(Max_age_M-a0)))*1.1, label = "Max age (M derived)") +
        ggplot2::annotate("text", x = max(growth_curve$Age)*0.98, y = max(growth_curve$Length)*(0.05+2*label_spacing_VB), hjust=1, vjust=0, label = paste0("L[infinity]==", round(Linf), "~cm"), parse = T)+
        ggplot2::annotate("text", x = max(growth_curve$Age)*0.98, y = max(growth_curve$Length)*(0.05+label_spacing_VB), hjust=1, vjust=0, label = paste0("k==", round(k,2), '/yr'), parse = T) +
        ggplot2::annotate("text", x = max(growth_curve$Age)*0.98, y = max(growth_curve$Length)*0.05, hjust=1, vjust=0, label = paste0('a[0]==',round(a0,2),'~yr'), parse = T)
    } else {
      growth_plot <- growth_plot +
        ggplot2::annotate("text", x = max(growth_curve$Age)*0.98, y = max(growth_curve$Length)*(0.05+2*label_spacing_VB), hjust=1, vjust=0, label = paste0("L[infinity]==", round(Linf), "~cm"), parse = T)+
        ggplot2::annotate("text", x = max(growth_curve$Age)*0.98, y = max(growth_curve$Length)*(0.05+label_spacing_VB), hjust=1, vjust=0, label = paste0("k==", round(k,2), '/yr'), parse = T) +
        ggplot2::annotate("text", x = max(growth_curve$Age)*0.98, y = max(growth_curve$Length)*0.05, hjust=1, vjust=0, label = paste0('a[0]==',round(a0,2),'~yr'), parse = T)
    }

    if (show_MLS_label) {
      growth_plot <- growth_plot +
        ggplot2::geom_hline(yintercept=MLS, linetype="dashed") +
        ggplot2::annotate("text", x = max(growth_curve$Age)*0.98, y = MLS+1, hjust=1, vjust=0, label = "Legal-sized")


    }

  }else {
    growth_plot <- ggplot2::ggplot() +
      ggplot2::ggtitle("von Bertalanffy growth curve") +
      ggplot2::annotate("text", x = 1, y = 1, label = "Insufficient information\navailable", size = point_size)+
      ggplot2::theme_void()

  }




  ## Mortality plot ----

  if(!is.null(M)){
    Max_age_M <-ceiling(5.4/M)
    mortality_curve <- data.frame(Age = seq(0,Max_age_M,0.1,), Survival = exp(-M * seq(0,Max_age_M,0.1)))

    Mortality_plot <- ggplot2::ggplot(mortality_curve, ggplot2::aes(Age, Survival))+
      ggplot2::ggtitle("Survivorship-to-age curve")+
      ggplot2::geom_line(col = colours[1], linewidth = line_width)+
      ggplot2::annotate("text", x = max(mortality_curve$Age)*0.95,y = 0.90, hjust=1, vjust=0, label = paste("M =", M))+
      ggplot2::annotate("text", x = max(mortality_curve$Age)*0.95, y = 0.90-label_spacing_M, hjust=1, vjust=0, label = paste("Max age =", Max_age_M))+

      ggplot2::scale_y_continuous(breaks = breaks, name = "Age class survival by M", expand = c(0,0),
                                  limits = c(0, 1))+
      ggplot2::scale_x_continuous(breaks = breaks, name = "Age (years)", expand = c(0,0))+
      ggplot2::theme_bw()

  } else {
    Mortality_plot <- ggplot2::ggplot() + ggplot2::ggtitle("Survivorship-to-age curve") +
      ggplot2::annotate("text", x = 1, y = 1, label = "Insufficient information\navailable", size = point_size)+
      ggplot2::theme_void()
  }

  ## Weight length relationship
  if(!any(sapply(list(WLa, WLb), is.null))){

    if(!is.null(Linf)){     weight_length_curve <- data.frame( Length = seq(0,ceiling(Linf),1)) |>
      dplyr::mutate(Weight = WLa*Length^WLb)
    } else if(!is.null(max_length)){
      weight_length_curve <- data.frame( Length = seq(0,ceiling(max_length),1)) |>
        dplyr::mutate(Weight = WLa*Length^WLb)
    } else {
      weight_length_curve <- data.frame( Length = seq(0,ceiling(100),1)) |>
        dplyr::mutate(Weight = WLa*Length^WLb)
    }

    if (missing(label_spacing_WL)) {label_spacing_WL <- max(weight_length_curve$Weight)*0.1}

    W_L_plot <- ggplot2::ggplot(weight_length_curve, ggplot2::aes(Length, Weight))+
      ggplot2::ggtitle("Weight-length relationship")+
      ggplot2::geom_line(col = colours[1], linewidth = line_width)+
      ggplot2::annotate("text", x = max(weight_length_curve$Length)*0.05,y = max(weight_length_curve$Weight), hjust=0, vjust=0, label = paste0("WL[b]==", round(WLb,2)), parse = T)+
      ggplot2::annotate("text", x = max(weight_length_curve$Length)*0.05,y = max(weight_length_curve$Weight)-label_spacing_WL, hjust=0, vjust=0, label = paste0("WL[a]==", round(WLa,6)), parse = T)+

      ggplot2::scale_y_continuous(breaks = breaks, name = "Weight (kg)", expand = c(0,0),
                                  limits = c(0, max(weight_length_curve$Weight)*1.1))+
      ggplot2::scale_x_continuous(breaks = breaks, name = "Length (cm)", expand = c(0,0))+
      ggplot2::theme_bw()

    if(!is.null(MLS))
      W_L_plot <- W_L_plot +
      ggplot2::annotate("rect",xmin = MLS, xmax = Inf, ymin = -Inf,ymax = Inf, alpha = 0.2,linetype = "dashed", fill = colours[2], col = "black")#+

    if(!any(sapply(list(L50,L95), is.null))){

      W_Mat50 <- WLa*L50^WLb
      W_Mat95 <- WLa*L95^WLb

      W_L_plot <- W_L_plot +
        ggplot2::annotate(geom="segment",x=L50, xend=L50, y=0, yend= W_Mat50, linetype = "dotted", colour=colours[4])+
        ggplot2::annotate(geom="segment",x=0, xend=L50, y=W_Mat50, yend= W_Mat50, linetype = "dotted", colour=colours[4])+
        ggplot2::annotate(geom="point",x=L50, y=W_Mat50,shape=16, size = point_size, col = colours[3])+
        ggplot2::annotate(geom="segment",x=L95, xend=L95, y=0, yend= W_Mat95, linetype = "dotted", colour=colours[4])+
        ggplot2::annotate(geom="segment",x=0, xend=L95, y=W_Mat95, yend= W_Mat95, linetype = "dotted", colour=colours[4])+
        ggplot2::annotate(geom="point",x=L95, y=W_Mat95,shape=17, size = point_size, col = colours[3])
    }

  } else {
    W_L_plot <- ggplot2::ggplot() + ggplot2::ggtitle("Weight-length relationship") +
      ggplot2::annotate("text", x = 1, y = 1, label = "Insufficient information\navailable", size = point_size)+
      ggplot2::theme_void()
  }

  ## Maturity
  if(!any(sapply(list(L50,L95), is.null))){

    if(!is.null(Linf)){ plot_length <- Linf
    } else if(!is.null(max_length)){
      plot_length <- max_length
    } else {
      plot_length <- 100
    }

    maturity_curve <- data.frame(Length = seq(0,ceiling(plot_length-1),1),
                                 Maturity = 1*(1+exp(-log(19)*((seq(0,ceiling(plot_length-1),1)-L50)/(L95-L50))))^-1) |>
      dplyr::mutate(marker = NA) |>
      rbind(data.frame(Length=L50,Maturity=0.5, marker = "L50")) |>
      rbind(data.frame(Length=L95,Maturity=0.95, marker = "L95"))

    Maturity_plot <- ggplot2::ggplot()+
      ggplot2::ggtitle("Length-at-maturity curve")+
      ggplot2::geom_line(data = maturity_curve |> dplyr::filter(is.na(marker)), ggplot2::aes(Length, Maturity), col = colours[1], linewidth = line_width)+
      ggplot2::annotate("segment",y=.5, yend=.5, x=0, xend= L50, linetype = "dotted", colour=colours[4])+
      ggplot2::annotate("segment",y=0, yend=.5, x=L50, xend= L50, linetype = "dotted", colour=colours[4])+
      ggplot2::annotate("point",x=L50, y=.50,shape=16, size = point_size, col = colours[3])+
      ggplot2::annotate("point",x=L95, y=.95,shape=17, size = point_size, col = colours[3])+
      ggplot2::annotate("segment",y=.95, yend=.95, x=0, xend= L95, linetype = "dotted", colour=colours[4])+
      ggplot2::annotate("segment",y=0, yend=.95, x=L95, xend= L95, linetype = "dotted", colour=colours[4])+

      ggplot2::scale_y_continuous(breaks = breaks, name = "Proportion mature",
                                  limits = c(0, 1), expand = c(0,0))+
      ggplot2::scale_x_continuous(breaks = breaks, name = "Length (cm)", expand = c(0,0))+
      ggplot2::theme_bw() +
      ggplot2::scale_shape_manual(name="", values=c(16,17)) +
      ggplot2::theme(legend.position="none") +
      ggplot2::annotate("text", x = max(maturity_curve$Length)*0.95, y = .05+label_spacing_mat, hjust=1, label = paste0("L[50]==", L50, "~cm"), parse = T) +
      ggplot2::annotate("text", x = max(maturity_curve$Length)*0.95, y = .05, hjust=1, label = paste0("L[95]==", L95, "~cm"), parse = T)


    if(!is.null(MLS))
      Maturity_plot <- Maturity_plot +
      ggplot2::annotate("rect",xmin = MLS, xmax = Inf, ymin = -Inf,ymax = Inf, alpha = 0.2,linetype = "dashed", fill = colours[2], col = "black")#+

  } else {
    Maturity_plot <- ggplot2::ggplot() + ggplot2::ggtitle("Length-at-maturity curve") +
      ggplot2::annotate("text", x = 1, y = 1, label = "Insufficient information\navailable", size = point_size)+
      ggplot2::theme_void()
  }


  # Combine ----

  title <- cowplot::ggdraw() + ggplot2::theme(
    plot.margin = ggplot2::margin(0, 0, 0, 7)
  )

  plot_row <- cowplot::plot_grid( growth_plot, Mortality_plot,W_L_plot, Maturity_plot, ncol = 2, align = "vh")

  cowplot::plot_grid(
    title, plot_row,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.1, 1)
  )


}
