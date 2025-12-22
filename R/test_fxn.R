testfxn <- function(data){  #data is the only argument passed to the "testfxn"
  smastex_filtered <- data %>%  #create a data object
    dplyr::filter(!Param %in% badparams) %>% #filter out any bad params


    dplyr::rename( #rename order details cols
      `Receipt Temp (⁰C)`      = any_of("OrderDetails_User1"),
      Comments                 = any_of("OrderDetails_User2"),
      `Depth (m)`              = any_of("OrderDetails_User3"),
      Replicate                = any_of("OrderDetails_User4"),
      `Mc_T Receipt Temp (⁰C)` = any_of("OrderDetails_User5")
    )
  return(smastex_filtered) #be sure to return the data object

}

testfxn(smast_ex)





