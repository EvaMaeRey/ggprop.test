kissing_data |> 
  ggplot() + 
  aes(x = outcome) + 
  geom_stack() + 
  geom_stack_label() + 
  geom_support() + 
  geom_prop() +
  geom_prop_label() + 
  stamp_prop(.5) + 
  stamp_prop_label(.5) + 
  geom_normal_prop_null() + 
  geom_normal_prop_null_sds()
