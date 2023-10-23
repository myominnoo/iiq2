#Load the required library 
library(tidyverse)
library(googlesheets4)

sheet1_id <- Sys.getenv("sheet1_id")
sheet2_id <- Sys.getenv("sheet2_id")


# ggplot theme ------------------------------------------------------------
ggplot2::theme_set(ggprism::theme_prism(base_size = 14))



# google sheet import -----------------------------------------------------

gs_name <- paste0(
	"https://docs.google.com/spreadsheets/d/", 
	sheet1_id
)


#Read google sheets data into R
ps_raw <- googlesheets4::read_sheet(gs_name, sheet = "Pre-Screening data")
ra_raw <- googlesheets4::read_sheet(gs_name, sheet = "Recruitment and Appointments")

gs_name <- paste0(
	"https://docs.google.com/spreadsheets/d/",
	sheet2_id
)
lab_raw <- googlesheets4::read_sheet(gs_name, sheet = "Prescreening", skip = 1)



# Pre-screening -----------------------------------------------------------

## PRE-SCREENING DATA SHEET
# MOBILIZATION AND PRE_SCREENING CASCADE
# Total number mobilized = Total number with a PS number on column B
# Total number pre-screened = Total number of samples collected “Yes” on column D
# Qualified for enrolment = Enroll “Yes” on column F
# Enrolled into the study = Total number with “IIQ” on column H
ps <- ps_raw |> 
	dplyr::select(
		ps_no = `PS No.`, 
		prescreened = `Sample Collected Yes/No`, 
		eligible = `Enrol Yes/No`,
		enrolled = `Study No.`, 
		reasons_no_ps = `If No, Why?`
	) |> 
	dplyr::mutate(
		across(c(prescreened, eligible), stringr::str_to_lower), 
		prescreened = dplyr::case_when(
			prescreened == "yes" ~ "Pre-screened", 
		), 
		eligible = dplyr::case_when(
			eligible == "yes" ~ "Eligible", 
		), 
	) |> 
	tidyr::drop_na(ps_no)

fig_ps_cas <- dplyr::bind_rows(
	ps |> dplyr::count() |> dplyr::mutate(name = "Total"),
	ps |> dplyr::count(prescreened) |> dplyr::select(name = prescreened, n),
	ps |> dplyr::count(eligible) |> dplyr::select(name = eligible, n),
	ps |> 
		dplyr::mutate(name = !is.na(enrolled)) |> 
		dplyr::count(name) |> 
		dplyr::filter(name) |> 
		dplyr::mutate(name = "Enrolled")
) |> 
	tidyr::drop_na() |>
	dplyr::mutate(
		name = factor(name, c("Total", "Pre-screened", "Eligible", "Enrolled")), 
		N = dplyr::lag(n), 
		pct = n / N, 
		pct = scales::label_percent()(pct), 
		label = ifelse(is.na(pct), n, paste0(n, " (", pct, ")"))
	) |> 
	ggplot(aes(x = name, y = n)) +  
	geom_bar(aes(fill = name), stat = "identity", show.legend = FALSE) + 
	geom_text(aes(label = label), size = 4, fontface = "bold", vjust = -0.5) + 
	scale_y_continuous(limits = c(0, 720)) +
	labs(x = "", y = "Count",
			 title = "Mobilization and Pre-screening Cascade")
fig_ps_cas
	

fig_ps_elim <- ps |> 
	dplyr::filter(is.na(prescreened)) |> 
	dplyr::mutate(
		reasons_no_ps = forcats::fct_infreq(reasons_no_ps),
		reasons_no_ps = forcats::fct_lump_n(reasons_no_ps, 10), 
		reasons_no_ps = forcats::fct_rev(reasons_no_ps)
	) |> 
	dplyr::count(reasons_no_ps) |> 
	tidyr::drop_na(reasons_no_ps) |> 
	ggplot(aes(x = n, y = reasons_no_ps)) + 
	geom_col(aes(fill = reasons_no_ps), show.legend = FALSE) + 
	geom_text(aes(label = n), hjust = -0.25) + 
	scale_x_continuous(limits = c(0, 150)) +
	labs(y = "", x = "Count", 
			 title = paste0(
			 	"Reasons for non-eligiblity\n(N = ", 
			 	(ps |> filter(is.na(prescreened)) |> nrow()), ")"
			 ))
fig_ps_elim





# Recruitment and Appointment ---------------------------------------------


fig_ra <- ra_raw |> 
	dplyr::select(pid = `Study No.`, starts_with("Participant Status")) |> 
	tidyr::pivot_longer(
		cols = -pid, names_to = "visit", values_to = "status"
	) |> 
	dplyr::mutate(
		visit = gsub("[^0-9.-]|...", "", visit), 
		visit = factor(as.numeric(visit), labels = paste("Visit", 1:8, sep = " "))
	) |> 
	dplyr::count(visit, status) |> 
	dplyr::mutate(
		status = if_else(is.na(status), "Pending", status), 
		status = factor(status), 
		status = forcats::fct_rev(status), 
	) |> 
	dplyr::group_by(visit) |>
	dplyr::mutate(
		N = sum(n), 
		pct = n / N, 
		label = paste0(n, " (", scales::label_percent(accuracy = 1)(pct), ")")
	) |> 
	ggplot(aes(x = visit, y = n)) + 
	geom_col(aes(fill = status), position = "stack", color = "black")	+
	geom_text(aes(label = label, group = status),
						size = 3.5, fontface = "bold", 
						position = position_stack(vjust = 0.5)) +
	labs(x = "", y = "Count", 
			 title = "Study Cascade: Recruitment & Appointment") + 
	theme(legend.position = "bottom")
fig_ra




# Lab data ----------------------------------------------------------------

lab_sheets <- c("Prescreening", "Clinic Follow Up", "Enrolment v1", 
								paste0("FollowUp v", 2:8))

lab <- purrr::map(lab_sheets, function(x) {
	if (x == "Enrolment v1") {
		df <- googlesheets4::read_sheet(gs_name, sheet = x, skip = 0)
	} else {
		df <- googlesheets4::read_sheet(gs_name, sheet = x, skip = 1)
	}
	if (x == "Prescreening") {
		df <- df |> 
			# glimpse() |> 
			dplyr::select(pid = 2, RPR, CT, NG, TV, BV, mlnum = `ML Number`, 
										enroll = `Enroll?`, enrolled = `Enrolment Status`) 
	} else {
		df <- df |> 
			dplyr::select(pid = 2, RPR, CT, NG, TV, BV) 
	}
	
	df |> 
		# dplyr::glimpse() |>
		dplyr::mutate(
			source = x, 
			across(RPR:BV, ~ if_else(.x == "-", NA, .x)), 
			across(RPR:TV, ~ if_else(.x == "Neg", "Negative", .x)), 
			BV = if_else(BV == "Internediate", "Intermediate", BV)
		)
}) |> 
	purrr::reduce(bind_rows)


fig_lab <- purrr::map(c("RPR", "CT", "NG", "TV", "BV"), function(x) {
	df <- lab |> 
		dplyr::select(result = all_of(x), source) |> 
		dplyr::group_by(source) |> 
		dplyr::count(result) |> 
		tidyr::drop_na(result) |> 
		dplyr::mutate(test = x) |> 
		dplyr::group_by(test, source) |> 
		dplyr::mutate(
			N = sum(n), 
			pct = n / N, 
			label = paste0(n, " (", scales::label_percent(accuracy = 1)(pct), ")")
		) |> 
		dplyr::ungroup()
}) |> 
	purrr::reduce(bind_rows) |> 
	dplyr::mutate(
		source = factor(source, levels = lab_sheets)
	) |> 
	ggplot(aes(x = test, y = n)) + 
	geom_col(aes(fill = result), color = "black", linewidth = .2) + 
	geom_text(aes(label = n, group = result),
						position = position_stack(vjust = 0.5)) +
	labs(x = NULL, y = "Count", 
			 title = "Cascade of Lab Results") + 
	facet_grid(~ source, switch = "both") + 
	theme(legend.position = "right", 
				strip.placement = "outside")
fig_lab


fig_retest <- lab |> 
	filter(source == "Prescreening") |> 
	mutate(retest = case_when(
		grepl("R", mlnum) ~ "Yes", 
		TRUE ~ "No"
		),
		retest = paste0("Retested: ", retest),
		enroll_id = enrolled, 
		enrolled = if_else(is.na(enrolled), "No", "Yes"), 
		enroll = if_else(is.na(enroll), "Missing", enroll),
		enroll = factor(enroll, c("Yes", "No", "Missing")),
		enrolled = if_else(enrolled == "Yes", "Enrolled", "Not Enrolled")
	) |> 
	count(retest, enroll, enrolled) |> 
	group_by(retest, enroll) |> 
	mutate(
		N = sum(n), 
		pct = n / N, 
		label = scales::label_percent(accuracy = 0.1)(pct), 
		label = paste0(n, ", ", label)
	) |> 
	ungroup() |> 
	ggplot(aes(x = enroll, y = pct)) +
	geom_col(aes(fill = enrolled)) +
	facet_grid(~ retest) +
	ggrepel::geom_label_repel(
		aes(label = label, 
				fill = enrolled)
	) + 
	scale_y_continuous(
		limits = c(0, 1), 
		labels = scales::label_percent()
	) + 
	labs(x = "Percentage",
			 y = "Qualified", 
			 title = "Retest Cascade")



	


