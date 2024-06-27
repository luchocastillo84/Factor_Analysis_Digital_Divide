## Codebook 

# Load the necessary library
library(tibble)

# Define the data
codebook <- tibble::tibble(
  Code = c(
    "A2_A2", "A1_B2b", "A2_C2", "A2_C5a", "A2_C4_low", "A2_C4_high", "S_B1", "S_B2a",
    "S_B5a1", "S_B5b1", "S_B5c1", "S_B5d1", "S_B5e1", "S_B5f1", "S_B5g1", "UMK_C7",
    "UC_C8a", "UMK_C8c", "UMK_C8h", "UMK_C9a", "UMK_C9c", "UM_E1", "UM_E2b", "UM_E2a",
    "UM_C8g", "UC_J7", "size_rev_small", "size_rev,medium", "size_rev_large"
  ),
  Variable_Name = c(
    "Percentage of employees using the computer out of the total employees",
    "IT training courses for employees without specialist ICT skills",
    "Percentage of employees using computers connected to the internet",
    "Enterprise provides mobile devices with mobile connection",
    "Internet download speed low",
    "Internet download speed high",
    "Employment of specialists in computer subjects",
    "IT training courses for employees with specialist ICT skills",
    "Use of internal personnel for ICT infrastructure maintenance",
    "Use of internal personnel for office software support",
    "Use of internal personnel for enterprise software development",
    "Use of internal personnel for enterprise software support",
    "Use of internal personnel for web development",
    "Use of internal personnel for web development support",
    "Use of internal personnel for IT security management",
    "Use of Website",
    "Possibility to place orders or reservations online e.g., online shopping cart",
    "Access to product catalogs or price lists",
    "Links or references to company profiles on social media",
    "Social network",
    "Social media and multimedia",
    "Using ERP software",
    "Use operational CRM software",
    "Use analytical CRM software",
    "Announcement of vacancies or possibility to apply for employment online",
    "Web sales through intermediary websites or eCommerce sites, marketplaces or apps",
    "Firm size small",
    "Firm size medium",
    "Firm size large"
  )
)

save(codebook, file = here("Data", "Processed", "codebook.rda"))

