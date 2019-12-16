open_project 07_vivado_project/CustomLogic.xpr

set CLOCK_FREQ_MHZ [lindex $argv 0]
set CLOCK_FREQ_HZ  [expr $CLOCK_FREQ_MHZ * 1000000]
set RST_FREQ [expr $CLOCK_FREQ_MHZ - 1]

source bigIP.tcl

# Add Spatial Top verilog module
set origin_dir [file dirname [file normalize [info script]]]
add_files -norecurse "$origin_dir/SpatialIP.v"
add_files -norecurse "$origin_dir/RetimeShiftRegister.sv"
add_files -norecurse "$origin_dir/AXI4LiteToRFBridgeVerilog.v"

launch_runs synth_1 -jobs 48
wait_on_run synth_1

open_run -name implDesign synth_1
report_timing_summary -file ./synth_timing_summary.rpt
report_utilization -packthru -file ./synth_utilization.rpt
report_utilization -packthru -hierarchical -hierarchical_depth 20 -hierarchical_percentages -file ./synth_utilization_hierarchical.rpt
report_ram_utilization -detail -file ./synth_ram_utilization.rpt

launch_runs impl_1 -jobs 48
wait_on_run impl_1
launch_runs impl_1 -to_step write_bitstream -jobs 6
wait_on_run impl_1

# Reports
open_run -name implDesign impl_1
report_timing_summary -file ./par_timing_summary.rpt
report_utilization -packthru -file  ./par_utilization.rpt
report_utilization -packthru -hierarchical -hierarchical_depth 20 -hierarchical_percentages -file  ./par_utilization_hierarchical.rpt
report_ram_utilization -detail -file ./par_ram_utilization.rpt
report_high_fanout_nets -ascending -timing -load_types -file ./par_high_fanout_nets.rpt

#Export bitstream
file copy -force ./07_vivado_project/CustomLogic.runs/impl_1/CustomLogicTop.bit ./accel.bit
