"""
!----------------------------------------------------------------------------------------------------------------------
!> @author
!> Daniel Mohedano Rodríguez
!
!> @date
!> 29 May 2025
!
! DESCRIPTION:
!> Automatically generated Julia interface module for formulation_terms_module_jl_interface
!----------------------------------------------------------------------------------------------------------------------
"""
_HOFEM_LIB_PATH = get(ENV, "HOFEM_LIB_PATH", "")

function thermal_formulation_set_delta_sim_time!(data_c_ptr::Ptr{Cvoid}, val::Cdouble)
	@ccall _HOFEM_LIB_PATH.thermal_formulation_set_delta_sim_time(data_c_ptr::Ptr{Cvoid}, val::Cdouble)::Cvoid
end

function thermal_formulation_get_delta_sim_time(data_c_ptr::Ptr{Cvoid})::Cdouble
	return @ccall _HOFEM_LIB_PATH.thermal_formulation_get_delta_sim_time(data_c_ptr::Ptr{Cvoid})::Cdouble
end

function thermal_formulation_set_stop_sim_time!(data_c_ptr::Ptr{Cvoid}, val::Cdouble)
	@ccall _HOFEM_LIB_PATH.thermal_formulation_set_stop_sim_time(data_c_ptr::Ptr{Cvoid}, val::Cdouble)::Cvoid
end

function thermal_formulation_get_stop_sim_time(data_c_ptr::Ptr{Cvoid})::Cdouble
	return @ccall _HOFEM_LIB_PATH.thermal_formulation_get_stop_sim_time(data_c_ptr::Ptr{Cvoid})::Cdouble
end

function thermal_formulation_set_current_sim_time!(data_c_ptr::Ptr{Cvoid}, val::Cdouble)
	@ccall _HOFEM_LIB_PATH.thermal_formulation_set_current_sim_time(data_c_ptr::Ptr{Cvoid}, val::Cdouble)::Cvoid
end

function thermal_formulation_get_current_sim_time(data_c_ptr::Ptr{Cvoid})::Cdouble
	return @ccall _HOFEM_LIB_PATH.thermal_formulation_get_current_sim_time(data_c_ptr::Ptr{Cvoid})::Cdouble
end

function thermal_formulation_set_total_time_steps!(data_c_ptr::Ptr{Cvoid}, val::Cint)
	@ccall _HOFEM_LIB_PATH.thermal_formulation_set_total_time_steps(data_c_ptr::Ptr{Cvoid}, val::Cint)::Cvoid
end

function thermal_formulation_get_total_time_steps(data_c_ptr::Ptr{Cvoid})::Cint
	return @ccall _HOFEM_LIB_PATH.thermal_formulation_get_total_time_steps(data_c_ptr::Ptr{Cvoid})::Cint
end

function thermal_formulation_set_step_tolerance!(data_c_ptr::Ptr{Cvoid}, val::Cdouble)
	@ccall _HOFEM_LIB_PATH.thermal_formulation_set_step_tolerance(data_c_ptr::Ptr{Cvoid}, val::Cdouble)::Cvoid
end

function thermal_formulation_get_step_tolerance(data_c_ptr::Ptr{Cvoid})::Cdouble
	return @ccall _HOFEM_LIB_PATH.thermal_formulation_get_step_tolerance(data_c_ptr::Ptr{Cvoid})::Cdouble
end

function thermal_formulation_set_time_step_strategy!(data_c_ptr::Ptr{Cvoid}, val::String)
	@ccall _HOFEM_LIB_PATH.thermal_formulation_set_time_step_strategy(data_c_ptr::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function thermal_formulation_get_time_step_strategy(data_c_ptr::Ptr{Cvoid}, val::String)
	@ccall _HOFEM_LIB_PATH.thermal_formulation_get_time_step_strategy(data_c_ptr::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function print_thermal_formulation(data_c_ptr::Ptr{Cvoid})
	@ccall _HOFEM_LIB_PATH.print_thermal_formulation(data_c_ptr::Ptr{Cvoid})::Cvoid
end

function print_double_curl_wave_formulation(data_c_ptr::Ptr{Cvoid})
	@ccall _HOFEM_LIB_PATH.print_double_curl_wave_formulation(data_c_ptr::Ptr{Cvoid})::Cvoid
end

function FormulationConstants_set_bilGradFiGradFi!(data_c_ptr::Ptr{Cvoid}, val::ComplexF64)
	@ccall _HOFEM_LIB_PATH.formulationconstants_set_bilgradfigradfi(data_c_ptr::Ptr{Cvoid}, val::ComplexF64)::Cvoid
end

function FormulationConstants_get_bilGradFiGradFi(data_c_ptr::Ptr{Cvoid})::ComplexF64
	return @ccall _HOFEM_LIB_PATH.formulationconstants_get_bilgradfigradfi(data_c_ptr::Ptr{Cvoid})::ComplexF64
end

function FormulationConstants_set_bilFiFi!(data_c_ptr::Ptr{Cvoid}, val::ComplexF64)
	@ccall _HOFEM_LIB_PATH.formulationconstants_set_bilfifi(data_c_ptr::Ptr{Cvoid}, val::ComplexF64)::Cvoid
end

function FormulationConstants_get_bilFiFi(data_c_ptr::Ptr{Cvoid})::ComplexF64
	return @ccall _HOFEM_LIB_PATH.formulationconstants_get_bilfifi(data_c_ptr::Ptr{Cvoid})::ComplexF64
end

function FormulationConstants_set_linFiFunction!(data_c_ptr::Ptr{Cvoid}, val::ComplexF64)
	@ccall _HOFEM_LIB_PATH.formulationconstants_set_linfifunction(data_c_ptr::Ptr{Cvoid}, val::ComplexF64)::Cvoid
end

function FormulationConstants_get_linFiFunction(data_c_ptr::Ptr{Cvoid})::ComplexF64
	return @ccall _HOFEM_LIB_PATH.formulationconstants_get_linfifunction(data_c_ptr::Ptr{Cvoid})::ComplexF64
end

function FormulationConstants_set_linBoundFiFunction!(data_c_ptr::Ptr{Cvoid}, val::ComplexF64)
	@ccall _HOFEM_LIB_PATH.formulationconstants_set_linboundfifunction(data_c_ptr::Ptr{Cvoid}, val::ComplexF64)::Cvoid
end

function FormulationConstants_get_linBoundFiFunction(data_c_ptr::Ptr{Cvoid})::ComplexF64
	return @ccall _HOFEM_LIB_PATH.formulationconstants_get_linboundfifunction(data_c_ptr::Ptr{Cvoid})::ComplexF64
end

function FormulationConstants_set_bilBoundFiFi!(data_c_ptr::Ptr{Cvoid}, val::ComplexF64)
	@ccall _HOFEM_LIB_PATH.formulationconstants_set_bilboundfifi(data_c_ptr::Ptr{Cvoid}, val::ComplexF64)::Cvoid
end

function FormulationConstants_get_bilBoundFiFi(data_c_ptr::Ptr{Cvoid})::ComplexF64
	return @ccall _HOFEM_LIB_PATH.formulationconstants_get_bilboundfifi(data_c_ptr::Ptr{Cvoid})::ComplexF64
end

function FormulationConstants_set_bilRotNiRotNi!(data_c_ptr::Ptr{Cvoid}, val::ComplexF64)
	@ccall _HOFEM_LIB_PATH.formulationconstants_set_bilrotnirotni(data_c_ptr::Ptr{Cvoid}, val::ComplexF64)::Cvoid
end

function FormulationConstants_get_bilRotNiRotNi(data_c_ptr::Ptr{Cvoid})::ComplexF64
	return @ccall _HOFEM_LIB_PATH.formulationconstants_get_bilrotnirotni(data_c_ptr::Ptr{Cvoid})::ComplexF64
end

function FormulationConstants_set_bilNiNi!(data_c_ptr::Ptr{Cvoid}, val::ComplexF64)
	@ccall _HOFEM_LIB_PATH.formulationconstants_set_bilnini(data_c_ptr::Ptr{Cvoid}, val::ComplexF64)::Cvoid
end

function FormulationConstants_get_bilNiNi(data_c_ptr::Ptr{Cvoid})::ComplexF64
	return @ccall _HOFEM_LIB_PATH.formulationconstants_get_bilnini(data_c_ptr::Ptr{Cvoid})::ComplexF64
end

function FormulationConstants_set_linBoundNiFunction!(data_c_ptr::Ptr{Cvoid}, val::ComplexF64)
	@ccall _HOFEM_LIB_PATH.formulationconstants_set_linboundnifunction(data_c_ptr::Ptr{Cvoid}, val::ComplexF64)::Cvoid
end

function FormulationConstants_get_linBoundNiFunction(data_c_ptr::Ptr{Cvoid})::ComplexF64
	return @ccall _HOFEM_LIB_PATH.formulationconstants_get_linboundnifunction(data_c_ptr::Ptr{Cvoid})::ComplexF64
end

function FormulationConstants_set_bilBoundNiNi!(data_c_ptr::Ptr{Cvoid}, val::ComplexF64)
	@ccall _HOFEM_LIB_PATH.formulationconstants_set_bilboundnini(data_c_ptr::Ptr{Cvoid}, val::ComplexF64)::Cvoid
end

function FormulationConstants_get_bilBoundNiNi(data_c_ptr::Ptr{Cvoid})::ComplexF64
	return @ccall _HOFEM_LIB_PATH.formulationconstants_get_bilboundnini(data_c_ptr::Ptr{Cvoid})::ComplexF64
end

function FormulationConstants_set_linNiFunction!(data_c_ptr::Ptr{Cvoid}, val::ComplexF64)
	@ccall _HOFEM_LIB_PATH.formulationconstants_set_linnifunction(data_c_ptr::Ptr{Cvoid}, val::ComplexF64)::Cvoid
end

function FormulationConstants_get_linNiFunction(data_c_ptr::Ptr{Cvoid})::ComplexF64
	return @ccall _HOFEM_LIB_PATH.formulationconstants_get_linnifunction(data_c_ptr::Ptr{Cvoid})::ComplexF64
end

function FormulationConstants_set_bilNiGradFi!(data_c_ptr::Ptr{Cvoid}, val::ComplexF64)
	@ccall _HOFEM_LIB_PATH.formulationconstants_set_bilnigradfi(data_c_ptr::Ptr{Cvoid}, val::ComplexF64)::Cvoid
end

function FormulationConstants_get_bilNiGradFi(data_c_ptr::Ptr{Cvoid})::ComplexF64
	return @ccall _HOFEM_LIB_PATH.formulationconstants_get_bilnigradfi(data_c_ptr::Ptr{Cvoid})::ComplexF64
end

function print_FormulationConstants(data_c_ptr::Ptr{Cvoid})
	@ccall _HOFEM_LIB_PATH.print_formulationconstants(data_c_ptr::Ptr{Cvoid})::Cvoid
end

function FormulationTerms_set_bilGradFiGradFi!(data_c_ptr::Ptr{Cvoid}, val::Cuchar)
	@ccall _HOFEM_LIB_PATH.formulationterms_set_bilgradfigradfi(data_c_ptr::Ptr{Cvoid}, val::Cuchar)::Cvoid
end

function FormulationTerms_get_bilGradFiGradFi(data_c_ptr::Ptr{Cvoid})::Cuchar
	return @ccall _HOFEM_LIB_PATH.formulationterms_get_bilgradfigradfi(data_c_ptr::Ptr{Cvoid})::Cuchar
end

function FormulationTerms_set_bilFiFi!(data_c_ptr::Ptr{Cvoid}, val::Cuchar)
	@ccall _HOFEM_LIB_PATH.formulationterms_set_bilfifi(data_c_ptr::Ptr{Cvoid}, val::Cuchar)::Cvoid
end

function FormulationTerms_get_bilFiFi(data_c_ptr::Ptr{Cvoid})::Cuchar
	return @ccall _HOFEM_LIB_PATH.formulationterms_get_bilfifi(data_c_ptr::Ptr{Cvoid})::Cuchar
end

function FormulationTerms_set_linFiFunction!(data_c_ptr::Ptr{Cvoid}, val::Cuchar)
	@ccall _HOFEM_LIB_PATH.formulationterms_set_linfifunction(data_c_ptr::Ptr{Cvoid}, val::Cuchar)::Cvoid
end

function FormulationTerms_get_linFiFunction(data_c_ptr::Ptr{Cvoid})::Cuchar
	return @ccall _HOFEM_LIB_PATH.formulationterms_get_linfifunction(data_c_ptr::Ptr{Cvoid})::Cuchar
end

function FormulationTerms_set_linBoundFiFunction!(data_c_ptr::Ptr{Cvoid}, val::Cuchar)
	@ccall _HOFEM_LIB_PATH.formulationterms_set_linboundfifunction(data_c_ptr::Ptr{Cvoid}, val::Cuchar)::Cvoid
end

function FormulationTerms_get_linBoundFiFunction(data_c_ptr::Ptr{Cvoid})::Cuchar
	return @ccall _HOFEM_LIB_PATH.formulationterms_get_linboundfifunction(data_c_ptr::Ptr{Cvoid})::Cuchar
end

function FormulationTerms_set_bilBoundFiFi!(data_c_ptr::Ptr{Cvoid}, val::Cuchar)
	@ccall _HOFEM_LIB_PATH.formulationterms_set_bilboundfifi(data_c_ptr::Ptr{Cvoid}, val::Cuchar)::Cvoid
end

function FormulationTerms_get_bilBoundFiFi(data_c_ptr::Ptr{Cvoid})::Cuchar
	return @ccall _HOFEM_LIB_PATH.formulationterms_get_bilboundfifi(data_c_ptr::Ptr{Cvoid})::Cuchar
end

function FormulationTerms_set_bilRotNiRotNi!(data_c_ptr::Ptr{Cvoid}, val::Cuchar)
	@ccall _HOFEM_LIB_PATH.formulationterms_set_bilrotnirotni(data_c_ptr::Ptr{Cvoid}, val::Cuchar)::Cvoid
end

function FormulationTerms_get_bilRotNiRotNi(data_c_ptr::Ptr{Cvoid})::Cuchar
	return @ccall _HOFEM_LIB_PATH.formulationterms_get_bilrotnirotni(data_c_ptr::Ptr{Cvoid})::Cuchar
end

function FormulationTerms_set_bilNiNi!(data_c_ptr::Ptr{Cvoid}, val::Cuchar)
	@ccall _HOFEM_LIB_PATH.formulationterms_set_bilnini(data_c_ptr::Ptr{Cvoid}, val::Cuchar)::Cvoid
end

function FormulationTerms_get_bilNiNi(data_c_ptr::Ptr{Cvoid})::Cuchar
	return @ccall _HOFEM_LIB_PATH.formulationterms_get_bilnini(data_c_ptr::Ptr{Cvoid})::Cuchar
end

function FormulationTerms_set_linNiFunction!(data_c_ptr::Ptr{Cvoid}, val::Cuchar)
	@ccall _HOFEM_LIB_PATH.formulationterms_set_linnifunction(data_c_ptr::Ptr{Cvoid}, val::Cuchar)::Cvoid
end

function FormulationTerms_get_linNiFunction(data_c_ptr::Ptr{Cvoid})::Cuchar
	return @ccall _HOFEM_LIB_PATH.formulationterms_get_linnifunction(data_c_ptr::Ptr{Cvoid})::Cuchar
end

function FormulationTerms_set_linBoundNiFunction!(data_c_ptr::Ptr{Cvoid}, val::Cuchar)
	@ccall _HOFEM_LIB_PATH.formulationterms_set_linboundnifunction(data_c_ptr::Ptr{Cvoid}, val::Cuchar)::Cvoid
end

function FormulationTerms_get_linBoundNiFunction(data_c_ptr::Ptr{Cvoid})::Cuchar
	return @ccall _HOFEM_LIB_PATH.formulationterms_get_linboundnifunction(data_c_ptr::Ptr{Cvoid})::Cuchar
end

function FormulationTerms_set_bilBoundNiNi!(data_c_ptr::Ptr{Cvoid}, val::Cuchar)
	@ccall _HOFEM_LIB_PATH.formulationterms_set_bilboundnini(data_c_ptr::Ptr{Cvoid}, val::Cuchar)::Cvoid
end

function FormulationTerms_get_bilBoundNiNi(data_c_ptr::Ptr{Cvoid})::Cuchar
	return @ccall _HOFEM_LIB_PATH.formulationterms_get_bilboundnini(data_c_ptr::Ptr{Cvoid})::Cuchar
end

function FormulationTerms_set_variational_unknown!(data_c_ptr::Ptr{Cvoid}, val::String)
	@ccall _HOFEM_LIB_PATH.formulationterms_set_variational_unknown(data_c_ptr::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function FormulationTerms_get_variational_unknown(data_c_ptr::Ptr{Cvoid}, val::String)
	@ccall _HOFEM_LIB_PATH.formulationterms_get_variational_unknown(data_c_ptr::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function FormulationTerms_set_basis_functions!(data_c_ptr::Ptr{Cvoid}, val::String)
	@ccall _HOFEM_LIB_PATH.formulationterms_set_basis_functions(data_c_ptr::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function FormulationTerms_get_basis_functions(data_c_ptr::Ptr{Cvoid}, val::String)
	@ccall _HOFEM_LIB_PATH.formulationterms_get_basis_functions(data_c_ptr::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function FormulationTerms_set_physics!(data_c_ptr::Ptr{Cvoid}, val::String)
	@ccall _HOFEM_LIB_PATH.formulationterms_set_physics(data_c_ptr::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function FormulationTerms_get_physics(data_c_ptr::Ptr{Cvoid}, val::String)
	@ccall _HOFEM_LIB_PATH.formulationterms_get_physics(data_c_ptr::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function print_FormulationTerms(data_c_ptr::Ptr{Cvoid})
	@ccall _HOFEM_LIB_PATH.print_formulationterms(data_c_ptr::Ptr{Cvoid})::Cvoid
end

function get_thermal_simulation()::Ptr{Cvoid}
	return @ccall _HOFEM_LIB_PATH.get_thermal_simulation()::Ptr{Cvoid}
end

function get_electromagnetic_simulation()::Ptr{Cvoid}
	return @ccall _HOFEM_LIB_PATH.get_electromagnetic_simulation()::Ptr{Cvoid}
end

function get_formulation_constants()::Ptr{Cvoid}
	return @ccall _HOFEM_LIB_PATH.get_formulation_constants()::Ptr{Cvoid}
end

function get_formulation_terms()::Ptr{Cvoid}
	return @ccall _HOFEM_LIB_PATH.get_formulation_terms()::Ptr{Cvoid}
end



#@C Anything inside this section will be preserved by the builder

#/@C
