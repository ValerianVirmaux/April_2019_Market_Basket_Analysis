
itemLabelToCategory <- function(itemLabel) {
  category <- ""
  if (itemLabel %in% c("Acer Aspire", "Alienware Laptop", "Apple MacBook Air", "Apple MacBook Pro", 
                       "ASUS Chromebook", "Dell Laptop", "Eluktronics Pro Gaming Laptop", "HP Laptop",
                       "HP Notebook Touchscreen Laptop PC", "LG Touchscreen Laptop")) {
    category <- "Laptops"
  } else if (itemLabel %in% c("Acer Desktop", "ASUS Desktop", "CYBERPOWER Gamer Desktop", 
                              "Dell 2 Desktop", "Dell Desktop", "HP Desktop", "iMac", "Intel Desktop",
                              "Lenovo Desktop Computer")) {
    category <- "Desktop"
  } else if (itemLabel %in% c("Acer Monitor", "AOC Monitor", "ASUS 2 Monitor", "ASUS Monitor", 
                              "Dell Monitor", "HP Monitor", "LG Monitor", "Samsung Monitor", 
                              "Sceptre Monitor", "ViewSonic Monitor")) {
    category <- "Monitors"
  } else if (itemLabel %in% c("3-Button Mouse", "Gaming Mouse Professional", "Generic Black 3-Button", 
                              "HP Wireless Mouse", "Logitech 3-button Mouse", "Logitech Wireless Mouse",
                              "Microsoft Basic Optical Mouse", "Redragon Gaming Mouse", "Slim Wireless Mouse",
                              "Wireless Portable Mouse")) {
    category <- "Computer Mice"
  } else if (itemLabel %in% c("Apple Magic Keyboard", "Apple Wired Keyboard", "Apple Wireless Keyboard",
                              "Backlit LED Gaming Keyboard", "Dell Wired Keyboard", "HP USB Keyboard",
                              "Logitech Keyboard", "Logitech Wireless Keyboard", "Rii LED Keyboard")) {
    category <- "Keyboard"
  } else if (itemLabel %in% c("Dell KM117 Wireless Keyboard & Mouse", 
                              "EagleTec Wireless Combo Keyboard and Mouse", 
                              "Logitech Desktop MK120 Mouse and keyboard Combo",
                              "Logitech MK270 Wireless Keyboard and Mouse Combo",
                              "Logitech MK360 Wireless Keyboard and Mouse Combo",
                              "Logitech MK550 Wireless Wave Keyboard and Mouse Combo",
                              "Microsoft Wireless Comfort Keyboard and Mouse",
                              "Microsoft Wireless Desktop Keyboard and Mouse",
                              "Rii LED Gaming Keyboard & Mouse Combo")) {
    category <- "Mouse and Keyboard Combo"
  } else if (itemLabel %in% c("Ailihen Stereo Headphones", "Kensington Headphones", "Koss Home Headphones",
                              "Logitech ClearChat Headset", "Logitech Stereo Headset", "Microsoft Headset",
                              "Panasonic On-Ear Stereo Headphones", "PC Gaming Headset", 
                              "XIBERIA Gaming Headset", "Zombie Gaming Headset")) {
    category <- "Computer Headphones"
  } else if (itemLabel %in% c("APIE Bluetooth Headphone", "Apple Earpods", "Monster Beats By Dr Dre",
                              "Otium Wireless Sports Bluetooth Headphone", "Panasonic In-Ear Headphone",
                              "Philips Flexible Earhook Headphone")) {
    category <- "Active Headphones"
  } else if (itemLabel %in% c("Audio Cable", "Etekcity Power Extension Cord Cable", "Ethernet Cable",
                              "HDMI Adapter", "HDMI Cable 6ft", "iPhone Charger Cable", 
                              "Samsung Charging Cable", "USB Cable", "VGA Monitor Cable")) {
    category <- "Computer Cords"
  } else if (itemLabel %in% c("Belkin Mouse Pad", "Computer Game", "Large Mouse Pad",
                              "Microsoft Office Home and Student 2016")) {
    category <- "Accessories"
  } else if (itemLabel %in% c("Bose Companion Speakers", "Cambridge Bluetooth Speaker", "Cyber Acoustics",
                              "DOSS Touch Wireless Bluetooth", "JBL Splashproof Portable Bluetooth Speaker",
                              "Logitech Multimedia Speakers", "Mackie CR Speakers", "Rokono Mini Speaker",
                              "Sonos")) {
    category <- "Speakers"
  } else if (itemLabel %in% c("Brother Printer", "Canon Office Printer", "DYMO Label Manker", "Epson Printer",
                              "HP Wireless Printer")) {
    category <- "Printers"
  } else if (itemLabel %in% c("Brother Printer Toner", "Canon Ink", "DYMO Labeling Tape", "Epson Black Ink",
                              "HP Black & Tri-color Ink")) {
    category <- "Printer Ink"
  } else if (itemLabel %in% c("Full Motion Monitor Mount", "Halter Acrylic Monitor Stand", 
                              "Halter Mesh Metal Monitor Stand", "Height-Adjustable Standing Desk",
                              "Multi Media Stand")) {
    category <- "Computer Stands"
  } else if (itemLabel %in% c("Fire HD Tablet", "iPad", "iPad Pro", "Kindle", "Samsung Galaxy Tablet")) {
    category <- "Computer Tablets"
  } else if (itemLabel %in% c("1TB Portable External Hard Drive", "2TB Portable External Hard Drive",
                              "3TB Portable External Hard Drive", "5TB Desktop Hard Drive",
                              "Slim 2TB Portable External Hard Drive")) {
    category <- "External Hardrives"
  } else if (itemLabel %in% c("Apple TV", "Fire TV Stick", "Google Home", "Roku Express", "Smart Light Bulb")) {
    category <- "Smart Home Devices"
  }
  
  category
}

itemToBlackWellCategory <- function(itemLabel) {
  category <- ""
  if (itemLabel %in% c("Acer Aspire", "Alienware Laptop", "Apple MacBook Air", "Apple MacBook Pro", 
                       "ASUS Chromebook", "Dell Laptop", "Eluktronics Pro Gaming Laptop", "HP Laptop",
                       "HP Notebook Touchscreen Laptop PC", "LG Touchscreen Laptop")) {
    category <- "Laptops"
  } else if (itemLabel %in% c("Acer Desktop", "ASUS Desktop", "CYBERPOWER Gamer Desktop", 
                              "Dell 2 Desktop", "Dell Desktop", "HP Desktop", "iMac", "Intel Desktop",
                              "Lenovo Desktop Computer")) {
    category <- "Desktop"
  } else if (itemLabel %in% c("Acer Monitor", "AOC Monitor", "ASUS 2 Monitor", "ASUS Monitor", 
                              "Dell Monitor", "HP Monitor", "LG Monitor", "Samsung Monitor", 
                              "Sceptre Monitor", "ViewSonic Monitor")) {
    category <- "Monitors"
  } else if (itemLabel %in% c("Belkin Mouse Pad", "Computer Game", "Large Mouse Pad",
                              "Microsoft Office Home and Student 2016", "3-Button Mouse", "Gaming Mouse Professional", 
                              "Generic Black 3-Button", "HP Wireless Mouse", "Logitech 3-button Mouse", "Logitech Wireless Mouse",
                              "Microsoft Basic Optical Mouse", "Redragon Gaming Mouse", "Slim Wireless Mouse",
                              "Wireless Portable Mouse", "Apple Magic Keyboard", "Apple Wired Keyboard", "Apple Wireless Keyboard",
                              "Backlit LED Gaming Keyboard", "Dell Wired Keyboard", "HP USB Keyboard",
                              "Logitech Keyboard", "Logitech Wireless Keyboard", "Rii LED Keyboard",
                              "Dell KM117 Wireless Keyboard & Mouse", 
                              "EagleTec Wireless Combo Keyboard and Mouse", 
                              "Logitech Desktop MK120 Mouse and keyboard Combo",
                              "Logitech MK270 Wireless Keyboard and Mouse Combo",
                              "Logitech MK360 Wireless Keyboard and Mouse Combo",
                              "Logitech MK550 Wireless Wave Keyboard and Mouse Combo",
                              "Microsoft Wireless Comfort Keyboard and Mouse",
                              "Microsoft Wireless Desktop Keyboard and Mouse",
                              "Rii LED Gaming Keyboard & Mouse Combo", 
                              "Ailihen Stereo Headphones", "Kensington Headphones", "Koss Home Headphones",
                              "Logitech ClearChat Headset", "Logitech Stereo Headset", "Microsoft Headset",
                              "Panasonic On-Ear Stereo Headphones", "PC Gaming Headset", 
                              "XIBERIA Gaming Headset", "Zombie Gaming Headset",
                              "Audio Cable", "Etekcity Power Extension Cord Cable", "Ethernet Cable",
                              "HDMI Adapter", "HDMI Cable 6ft", "iPhone Charger Cable", 
                              "Samsung Charging Cable", "USB Cable", "VGA Monitor Cable",
                              "Bose Companion Speakers", "Cambridge Bluetooth Speaker", "Cyber Acoustics",
                              "DOSS Touch Wireless Bluetooth", "JBL Splashproof Portable Bluetooth Speaker",
                              "Logitech Multimedia Speakers", "Mackie CR Speakers", "Rokono Mini Speaker",
                              "Sonos",
                              "APIE Bluetooth Headphone", "Apple Earpods", "Monster Beats By Dr Dre",
                              "Otium Wireless Sports Bluetooth Headphone", "Panasonic In-Ear Headphone",
                              "Philips Flexible Earhook Headphone",
                              "Full Motion Monitor Mount", "Halter Acrylic Monitor Stand", 
                              "Halter Mesh Metal Monitor Stand", "Height-Adjustable Standing Desk",
                              "Multi Media Stand",
                              "1TB Portable External Hard Drive", "2TB Portable External Hard Drive",
                              "3TB Portable External Hard Drive", "5TB Desktop Hard Drive",
                              "Slim 2TB Portable External Hard Drive")) {
    category <- "Accessories"
  } else if (itemLabel %in% c("Brother Printer", "Canon Office Printer", "DYMO Label Manker", "Epson Printer",
                              "HP Wireless Printer")) {
    category <- "Printers"
  } else if (itemLabel %in% c("Brother Printer Toner", "Canon Ink", "DYMO Labeling Tape", "Epson Black Ink",
                              "HP Black & Tri-color Ink")) {
    category <- "Printer Ink"
  } else if (itemLabel %in% c("Fire HD Tablet", "iPad", "iPad Pro", "Kindle", "Samsung Galaxy Tablet")) {
    category <- "Computer Tablets"
  } else if (itemLabel %in% c("Apple TV", "Fire TV Stick", "Google Home", "Roku Express", "Smart Light Bulb")) {
    category <- "Smart Home Devices"
  }
  
  category
}

toCategoryCol <- function(itemsCol, itemToCategoryFunction) {
  catCol <- sapply(itemsCol, itemToCategoryFunction)
  names(catCol) <- NULL
  catCol
}

addCategory <- function(transaction, category = "regular") {
  
  categoryFunction <- NULL
  if (category == "regular") {
    categoryFunction <- itemLabelToCategory
  } else if (category == "blackwells") {
    categoryFunction <- itemToBlackWellCategory
  } else {
    categoryFunction <- itemLabelToCategory
  }
  
  transItemInfo <- transaction@itemInfo
  transItemInfo$category <- toCategoryCol(transItemInfo$labels, categoryFunction)
  transaction@itemInfo <- transItemInfo
  transactionsCategories <- aggregate(transaction, by = "category")
  transactionsCategories
}