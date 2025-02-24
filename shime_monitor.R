# shime_monitor
# ver. 1.0.0
# 

check_and_print <- function(condition, message, error_message, should_print = TRUE) {
  if (should_print) {
    print(message)
  }
  if (!condition) {
    stop(error_message)
  }
}

print("Loading libraries...")
required_packages <- c("yaml", "dplyr", "ggplot2", "scales")
for (package in required_packages) {
  if (!require(package, character.only = TRUE)) {
    stop(paste("Package", package, "is required but not installed."))
  }
}

print("Reading config file...")
tryCatch({
  config <- yaml::read_yaml("config.yaml")
  print("Config loaded successfully")
  print(paste("Project:", config$PROJECT))
  print(paste("Type:", config$TYPE))
}, error = function(e) {
  stop(paste("Error reading config file:", e$message))
})

check_and_print(
  !is.null(config$TREATMENTS),
  "Checking treatments configuration...checked",
  "TREATMENTS not found in config file"
)
check_and_print(
  !is.null(config$PUMPS),
  "Checking pumps configuration...checked.",
  "PUMPS not found in config file"
)

project_name <- config$PROJECT
type <- config$TYPE
file_path <- config$MAIN_INPUT_FILE
separator <- config$main_separator
shime_type <- config$SHIME
treatments <- config$TREATMENTS
pumps_list <- config$PUMPS

start_date <- as.POSIXct(paste(config$START, "00:00:00"), format="%d-%m-%Y %H:%M:%S")
end_date <- as.POSIXct(paste(config$END, "23:59:59"), format="%d-%m-%Y %H:%M:%S")

if (!is.null(config$working_dir)) setwd(config$working_dir)

pump_data <- read.csv(file_path, sep = separator, header = TRUE)

pump_data$Volume <- as.numeric(gsub(",", ".", pump_data$Volume))

pump_data$LogDate <- as.POSIXct(pump_data$LogDate, format="%d-%m-%Y %H:%M:%S")

filtered_data <- pump_data %>% filter(LogDate >= start_date & LogDate <= end_date)

pumps_names <- setNames(
  sapply(pumps_list, function(x) sub("^[0-9]+ ", "", x)),
  sapply(pumps_list, function(x) sub(" .*", "", x))
)

filtered_data$PumpNames <- pumps_names[as.character(filtered_data$PumpNr)]

normalize_pump_name <- function(name) {
  name <- gsub("acid\\s*PC", "Acid PC", name, ignore.case = TRUE)
  name <- gsub("base\\s*PC", "Base PC", name, ignore.case = TRUE)
  name <- gsub("acid\\s*DC", "Acid DC", name, ignore.case = TRUE)
  name <- gsub("base\\s*DC", "Base DC", name, ignore.case = TRUE)
  return(name)
}

filtered_data$NormalizedPumpNames <- sapply(filtered_data$PumpNames, normalize_pump_name)

filtered_data <- filtered_data %>% filter(!is.na(NormalizedPumpNames))

filtered_data$PumpType <- ifelse(grepl("PC", filtered_data$NormalizedPumpNames), "PC", "DC")

time_breaks <- if (type == "inoculation") "1 hour" else "24 hours"

date_label_format <- if (type == "run") {
  "%d/%m/%Y"
} else if (type == "inoculation") {
  "%d/%m/%Y %H:%M"
} else {
  "%d/%m/%Y %H:%M:%S"
}

color_palette <- c(
    "#00688B", "#8FBC8F", "#B22222", "#B8860B", 
    "#4682B4", "#556B2F", "#CD5C5C", "#D2B48C",
    "#2F4F4F", "#A0522D", "#607B8B", "#698B69", 
    "#8B3A3A", "#8B7355", "#5F9EA0", "#6B8E23", 
    "#BC8F8F", "#DEB887", "#008B8B", "#8B4513"
)

if (is.null(treatments)) {
  stop("Error: 'TREATMENTS' is missing in the YAML file.")
}
treatment_map <- setNames(color_palette, treatments)
#
# ==============================================================================
# ==============================================================================
#
if (shime_type == "quad") {
    expected_numbers <- 1:4
    actual_numbers <- unique(as.numeric(sub(".*([1-4])$", "\\1", 
                                        filtered_data$NormalizedPumpNames)))
    
    missing_numbers <- setdiff(expected_numbers, actual_numbers)
    if (length(missing_numbers) > 0) {
        warning(paste("Missing pump numbers:", 
                     paste(missing_numbers, collapse = ", ")))
    }
    
    filtered_data$Treatment <- NA
    
    for (i in 1:4) {
        mask <- grepl(paste0(i, "$"), filtered_data$NormalizedPumpNames)
        filtered_data$Treatment[mask] <- treatments[i]
        # Removed print statement that was here
    }
    
    na_count <- sum(is.na(filtered_data$Treatment))
    if (na_count > 0) {
        warning(paste("Found", na_count, "rows with unassigned treatments"))
    }
    
    filtered_data$Treatment <- factor(filtered_data$Treatment, levels = treatments)

    acid_pumps_data <- filtered_data %>% filter(grepl("Acid", NormalizedPumpNames))
    base_pumps_data <- filtered_data %>% filter(grepl("Base", NormalizedPumpNames))
#
# --------------------------------------------------------------------------------
#
p_acid <- ggplot(acid_pumps_data, aes(x = LogDate, y = Volume, color = Treatment, group = NormalizedPumpNames)) +
    geom_line() +
    geom_point(size = 2.5) +
    labs(title = paste(project_name,",", type,",", "acid pumps", "\n", format(start_date, "%d/%m/%Y"), "to", format(end_date, "%d/%m/%Y")),
         x = "Log Date", y = "Volume", color = "Treatment") +
    scale_x_datetime(date_labels = date_label_format, 
                     breaks = seq(start_date, end_date, by = time_breaks)) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
    scale_color_manual(values = treatment_map) +
    facet_wrap(~ PumpType, scales = "free_y") +
    guides(color = guide_legend(title = "Treatment"))

p_base <- ggplot(base_pumps_data, aes(x = LogDate, y = Volume, color = Treatment, group = NormalizedPumpNames)) +
    geom_line() +
    geom_point(size = 2.5) +
    labs(title = paste(project_name,",", type,",", "base pumps", "\n", format(start_date, "%d/%m/%Y"), "to", format(end_date, "%d/%m/%Y")),
         x = "Log Date", y = "Volume", color = "Treatment") +
    scale_x_datetime(date_labels = date_label_format, 
                     breaks = seq(start_date, end_date, by = time_breaks)) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
    scale_color_manual(values = treatment_map) +
    facet_wrap(~ PumpType, scales = "free_y") +
    guides(color = guide_legend(title = "Treatment"))
#
# ==============================================================================
# ==============================================================================
#
} else {
    acid_pumps_data <- filtered_data %>% filter(grepl("Acid", NormalizedPumpNames))
    base_pumps_data <- filtered_data %>% filter(grepl("Base", NormalizedPumpNames))
    
    get_number <- function(name) {
        number <- sub(".*(?:DC|PC)([0-9]+).*", "\\1", name, perl = TRUE)
        as.numeric(number)
    }
    
    get_treatment_name <- function(name) {
        number <- get_number(name)
        if(!is.na(number) && number <= length(treatments)) {
            return(treatments[number])
        }
        return(name)
    }
    
    acid_pumps_data <- acid_pumps_data %>%
        mutate(Treatment = factor(sapply(NormalizedPumpNames, get_treatment_name), levels = treatments))
    base_pumps_data <- base_pumps_data %>%
        mutate(Treatment = factor(sapply(NormalizedPumpNames, get_treatment_name), levels = treatments))
    
    treatment_color_map <- setNames(color_palette[1:length(treatments)], treatments)

# --------------------------------------------------------------------------------
#
    p_acid <- ggplot(acid_pumps_data, aes(x = LogDate, y = Volume, color = Treatment)) +
        geom_line() +
        geom_point(size = 3) +
        labs(title = paste(project_name,",", type,",", "acid pumps", "\n", format(start_date, "%d/%m/%Y"), "to", format(end_date, "%d/%m/%Y")),
             x = "Log Date", y = "Volume", color = "Treatment") +
        scale_x_datetime(date_labels = date_label_format, breaks = seq(start_date, end_date, by = time_breaks)) + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_color_manual(values = treatment_color_map) +
        facet_wrap(~ PumpType)

    p_base <- ggplot(base_pumps_data, aes(x = LogDate, y = Volume, color = Treatment)) +
        geom_line() +
        geom_point(size = 3) +
        labs(title = paste(project_name,",", type,",", "base pumps", "\n", format(start_date, "%d/%m/%Y"), "to", format(end_date, "%d/%m/%Y")),
             x = "Log Date", y = "Volume", color = "Treatment") +
        scale_x_datetime(date_labels = date_label_format, breaks = seq(start_date, end_date, by = time_breaks)) + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_color_manual(values = treatment_color_map) +
        facet_wrap(~ PumpType)
}
#
# ==============================================================================
# ==============================================================================
#
output_dir <- file.path(getwd(), "output")
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}
#
# ==============================================================================
# ==============================================================================
#
ph_pumps_list <- config$PH_PUMPS

ph_probes_names <- setNames(
  sapply(ph_pumps_list, function(x) {
    label <- toupper(sub("^[0-9]+ *", "", x))
    label <- gsub("\\s+", "", label)
    label <- sub("(PC|DC)\\s*([0-9]+)", "\\1\\2", label, ignore.case = TRUE)
    return(label)
  }),
  sapply(ph_pumps_list, function(x) sub(" .*", "", x))
)

ph_data <- read.csv(config$PH_INPUT_FILE, sep = config$ph_separator, header = TRUE)

ph_data$Ph <- as.numeric(ph_data$Ph)

ph_data$LogDate <- as.POSIXct(ph_data$LogDate, format="%d-%m-%Y %H:%M:%S")

filtered_ph_data <- ph_data %>% 
  filter(LogDate >= start_date & LogDate <= end_date)

filtered_ph_data$ProbeNames <- ph_probes_names[as.character(filtered_ph_data$ProbeNr)]

filtered_ph_data$ProbeType <- case_when(
  grepl("PC", toupper(filtered_ph_data$ProbeNames)) ~ "PC",
  grepl("DC", toupper(filtered_ph_data$ProbeNames)) ~ "DC",
  TRUE ~ NA_character_
)

#
# ==============================================================================
# ==============================================================================
#
if (shime_type == "quad") {
  get_probe_number <- function(name) {
    as.numeric(sub(".*([1-4])$", "\\1", name))
  }
  
  filtered_ph_data$Treatment <- sapply(filtered_ph_data$ProbeNames, function(name) {
    num <- get_probe_number(name)
    if (!is.na(num) && num <= length(treatments)) {
      return(treatments[num])
    }
    return(NA)
  })
  
  filtered_ph_data$Treatment <- factor(filtered_ph_data$Treatment, levels = treatments)
#
# --------------------------------------------------------------------------------
#
  ph_alone <- ggplot(filtered_ph_data, aes(x = LogDate, y = Ph, color = Treatment)) +
    geom_line() +
    geom_point(size = 1) +
    labs(title = paste(project_name,",", type,",", "pH log", "\n", format(start_date, "%d/%m/%Y"), "to", format(end_date, "%d/%m/%Y")),
         x = "Log Date", y = "pH", color = "Treatment") +
    scale_x_datetime(date_labels = date_label_format, 
                    breaks = seq(start_date, end_date, by = time_breaks)) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
    scale_color_manual(values = treatment_map) +  
    facet_wrap(~ ProbeType, scales = "free_y") +
    guides(color = guide_legend(title = "Treatment"))
#
# --------------------------------------------------------------------------------
#
individual_ph_plots <- ggplot(filtered_ph_data, aes(x = LogDate, y = Ph, color = Treatment)) +
  geom_line() +
  geom_point(size = 1) +
  labs(title = paste(project_name,",", type,",", "individual pH logs", "\n", format(start_date, "%d/%m/%Y"), "to", format(end_date, "%d/%m/%Y")),
       x = "Log Date", y = "pH", color = "Treatment") +
  scale_x_datetime(date_labels = date_label_format, 
                  breaks = seq(start_date, end_date, by = time_breaks)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
  scale_color_manual(values = treatment_map) +
  # Create compound faceting variable that combines Treatment and ProbeType
  facet_wrap(~ interaction(Treatment, ProbeType, sep = " - "), scales = "free_y", ncol = 2) +
  guides(color = "none")
#
# ==============================================================================
# ==============================================================================
#
} else {
    filtered_ph_data <- filtered_ph_data %>%
        mutate(Treatment = factor(sapply(ProbeNames, get_treatment_name), levels = treatments))

    treatment_color_map <- setNames(color_palette[1:length(treatments)], treatments)
#
# --------------------------------------------------------------------------------
#
    ph_alone <- ggplot(filtered_ph_data, aes(x = LogDate, y = Ph, color = Treatment)) +
        geom_line() +
        geom_point(size = 2.5) +
        labs(title = paste(project_name,",", type,",", "pH log", "\n", format(start_date, "%d/%m/%Y"), "to", format(end_date, "%d/%m/%Y")),
             x = "Log Date", y = "pH", color = "Treatment") +
        scale_x_datetime(date_labels = date_label_format,
                        breaks = seq(start_date, end_date, by = time_breaks)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
        scale_color_manual(values = treatment_color_map) +
        facet_wrap(~ ProbeType, scales = "free_y")
#
# --------------------------------------------------------------------------------
#
    individual_ph_plots <- ggplot(filtered_ph_data, aes(x = LogDate, y = Ph, color = Treatment)) +
        geom_line() +
        geom_point(size = 2.5) +
        labs(title = paste(project_name,",", type,",", "individual pH logs", "\n", format(start_date, "%d/%m/%Y"), "to", format(end_date, "%d/%m/%Y")),
             x = "Log Date", y = "pH", color = "Treatment") +
        scale_x_datetime(date_labels = date_label_format,
                        breaks = seq(start_date, end_date, by = time_breaks)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
        scale_color_manual(values = treatment_color_map) +
        facet_wrap(~ Treatment, scales = "free_y", ncol = 2) +
        guides(color = "none")
}
#
# ==============================================================================
# ==============================================================================
#
ph_filename <- file.path(output_dir, paste0(project_name, "_", type, "_pH", ".png"))
individual_ph_filename <- file.path(output_dir, paste0(project_name, "_", type, "_individual_pH_logs", ".png"))

ggsave(filename = ph_filename, plot = ph_alone, width = 12, height = 6, dpi = 300)
ggsave(filename = individual_ph_filename, plot = individual_ph_plots, width = 15, height = 12, dpi = 300)

acid_plot_filename <- file.path(output_dir, paste0(project_name, "_", type, "_acid_pumps", ".png"))
base_plot_filename <- file.path(output_dir, paste0(project_name, "_", type, "_base_pumps", ".png"))

ggsave(filename = acid_plot_filename, plot = p_acid, width = 12, height = 6, dpi = 300)
ggsave(filename = base_plot_filename, plot = p_base, width = 12, height = 6, dpi = 300)
