# xml <- read_xml("lsu.xml")
#
# prep <- prepare(xml)
#
# parse_xml(.xml_list = prep[[1]])
#
# result <- map_dfr(.x = prep, .f = parse_xml, .id = "id") %>%
#   pivot_wider(names_from = name, values_from = value,
#               values_fn = list) %>%
#   unnest(cols = everything())
