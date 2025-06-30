"""
!----------------------------------------------------------------------------------------------------------------------
!> @author
!> Daniel Mohedano Rodríguez
!
!> @date
!> 30 June 2025
!
! DESCRIPTION:
!> Automatically generated Julia interface module for formulation_terms_module_jl_interface
!----------------------------------------------------------------------------------------------------------------------
"""

_HOFEM_LIB_PATH = get(ENV, "HOFEM_LIB_PATH", "")
#@C Anything inside this section will be preserved by the builder

#/@C

struct thermal_formulation
	handle::Ptr{Cvoid}
end

function thermal_formulation()
	return thermal_formulation(@ccall _HOFEM_LIB_PATH.new_thermal_formulation()::Ptr{Cvoid})
end

function Base.finalize(obj::thermal_formulation)
	@ccall _HOFEM_LIB_PATH.free_thermal_formulation(obj.handle::Ptr{Cvoid})::Cvoid
end

function set_delta_sim_time!(obj::thermal_formulation, val::Cdouble)
	@ccall _HOFEM_LIB_PATH.thermal_formulation_set_delta_sim_time(obj.handle::Ptr{Cvoid}, val::Cdouble)::Cvoid
end

function get_delta_sim_time(obj::thermal_formulation)::Cdouble
	return @ccall _HOFEM_LIB_PATH.thermal_formulation_get_delta_sim_time(obj.handle::Ptr{Cvoid})::Cdouble
end

function set_stop_sim_time!(obj::thermal_formulation, val::Cdouble)
	@ccall _HOFEM_LIB_PATH.thermal_formulation_set_stop_sim_time(obj.handle::Ptr{Cvoid}, val::Cdouble)::Cvoid
end

function get_stop_sim_time(obj::thermal_formulation)::Cdouble
	return @ccall _HOFEM_LIB_PATH.thermal_formulation_get_stop_sim_time(obj.handle::Ptr{Cvoid})::Cdouble
end

function set_current_sim_time!(obj::thermal_formulation, val::Cdouble)
	@ccall _HOFEM_LIB_PATH.thermal_formulation_set_current_sim_time(obj.handle::Ptr{Cvoid}, val::Cdouble)::Cvoid
end

function get_current_sim_time(obj::thermal_formulation)::Cdouble
	return @ccall _HOFEM_LIB_PATH.thermal_formulation_get_current_sim_time(obj.handle::Ptr{Cvoid})::Cdouble
end

function set_total_time_steps!(obj::thermal_formulation, val::Cint)
	@ccall _HOFEM_LIB_PATH.thermal_formulation_set_total_time_steps(obj.handle::Ptr{Cvoid}, val::Cint)::Cvoid
end

function get_total_time_steps(obj::thermal_formulation)::Cint
	return @ccall _HOFEM_LIB_PATH.thermal_formulation_get_total_time_steps(obj.handle::Ptr{Cvoid})::Cint
end

function set_step_tolerance!(obj::thermal_formulation, val::Cdouble)
	@ccall _HOFEM_LIB_PATH.thermal_formulation_set_step_tolerance(obj.handle::Ptr{Cvoid}, val::Cdouble)::Cvoid
end

function get_step_tolerance(obj::thermal_formulation)::Cdouble
	return @ccall _HOFEM_LIB_PATH.thermal_formulation_get_step_tolerance(obj.handle::Ptr{Cvoid})::Cdouble
end

function set_time_step_strategy!(obj::thermal_formulation, val::String)
	@ccall _HOFEM_LIB_PATH.thermal_formulation_set_time_step_strategy(obj.handle::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function get_time_step_strategy(obj::thermal_formulation, val::String)
	@ccall _HOFEM_LIB_PATH.thermal_formulation_get_time_step_strategy(obj.handle::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function print_thermal_formulation(obj::thermal_formulation)
	@ccall _HOFEM_LIB_PATH.print_thermal_formulation(obj.handle::Ptr{Cvoid})::Cvoid
end

struct double_curl_wave_formulation
	handle::Ptr{Cvoid}
end

function double_curl_wave_formulation()
	return double_curl_wave_formulation(@ccall _HOFEM_LIB_PATH.new_double_curl_wave_formulation()::Ptr{Cvoid})
end

function Base.finalize(obj::double_curl_wave_formulation)
	@ccall _HOFEM_LIB_PATH.free_double_curl_wave_formulation(obj.handle::Ptr{Cvoid})::Cvoid
end

function print_double_curl_wave_formulation(obj::double_curl_wave_formulation)
	@ccall _HOFEM_LIB_PATH.print_double_curl_wave_formulation(obj.handle::Ptr{Cvoid})::Cvoid
end

struct FormulationConstants
	handle::Ptr{Cvoid}
end

function FormulationConstants()
	return FormulationConstants(@ccall _HOFEM_LIB_PATH.new_formulationconstants()::Ptr{Cvoid})
end

function Base.finalize(obj::FormulationConstants)
	@ccall _HOFEM_LIB_PATH.free_formulationconstants(obj.handle::Ptr{Cvoid})::Cvoid
end

function set_bilGradFiGradFi!(obj::FormulationConstants, val::ComplexF64)
	@ccall _HOFEM_LIB_PATH.formulationconstants_set_bilgradfigradfi(obj.handle::Ptr{Cvoid}, val::ComplexF64)::Cvoid
end

function get_bilGradFiGradFi(obj::FormulationConstants)::ComplexF64
	return @ccall _HOFEM_LIB_PATH.formulationconstants_get_bilgradfigradfi(obj.handle::Ptr{Cvoid})::ComplexF64
end

function set_bilFiFi!(obj::FormulationConstants, val::ComplexF64)
	@ccall _HOFEM_LIB_PATH.formulationconstants_set_bilfifi(obj.handle::Ptr{Cvoid}, val::ComplexF64)::Cvoid
end

function get_bilFiFi(obj::FormulationConstants)::ComplexF64
	return @ccall _HOFEM_LIB_PATH.formulationconstants_get_bilfifi(obj.handle::Ptr{Cvoid})::ComplexF64
end

function set_linFiFunction!(obj::FormulationConstants, val::ComplexF64)
	@ccall _HOFEM_LIB_PATH.formulationconstants_set_linfifunction(obj.handle::Ptr{Cvoid}, val::ComplexF64)::Cvoid
end

function get_linFiFunction(obj::FormulationConstants)::ComplexF64
	return @ccall _HOFEM_LIB_PATH.formulationconstants_get_linfifunction(obj.handle::Ptr{Cvoid})::ComplexF64
end

function set_linBoundFiFunction!(obj::FormulationConstants, val::ComplexF64)
	@ccall _HOFEM_LIB_PATH.formulationconstants_set_linboundfifunction(obj.handle::Ptr{Cvoid}, val::ComplexF64)::Cvoid
end

function get_linBoundFiFunction(obj::FormulationConstants)::ComplexF64
	return @ccall _HOFEM_LIB_PATH.formulationconstants_get_linboundfifunction(obj.handle::Ptr{Cvoid})::ComplexF64
end

function set_bilBoundFiFi!(obj::FormulationConstants, val::ComplexF64)
	@ccall _HOFEM_LIB_PATH.formulationconstants_set_bilboundfifi(obj.handle::Ptr{Cvoid}, val::ComplexF64)::Cvoid
end

function get_bilBoundFiFi(obj::FormulationConstants)::ComplexF64
	return @ccall _HOFEM_LIB_PATH.formulationconstants_get_bilboundfifi(obj.handle::Ptr{Cvoid})::ComplexF64
end

function set_bilRotNiRotNi!(obj::FormulationConstants, val::ComplexF64)
	@ccall _HOFEM_LIB_PATH.formulationconstants_set_bilrotnirotni(obj.handle::Ptr{Cvoid}, val::ComplexF64)::Cvoid
end

function get_bilRotNiRotNi(obj::FormulationConstants)::ComplexF64
	return @ccall _HOFEM_LIB_PATH.formulationconstants_get_bilrotnirotni(obj.handle::Ptr{Cvoid})::ComplexF64
end

function set_bilNiNi!(obj::FormulationConstants, val::ComplexF64)
	@ccall _HOFEM_LIB_PATH.formulationconstants_set_bilnini(obj.handle::Ptr{Cvoid}, val::ComplexF64)::Cvoid
end

function get_bilNiNi(obj::FormulationConstants)::ComplexF64
	return @ccall _HOFEM_LIB_PATH.formulationconstants_get_bilnini(obj.handle::Ptr{Cvoid})::ComplexF64
end

function set_linBoundNiFunction!(obj::FormulationConstants, val::ComplexF64)
	@ccall _HOFEM_LIB_PATH.formulationconstants_set_linboundnifunction(obj.handle::Ptr{Cvoid}, val::ComplexF64)::Cvoid
end

function get_linBoundNiFunction(obj::FormulationConstants)::ComplexF64
	return @ccall _HOFEM_LIB_PATH.formulationconstants_get_linboundnifunction(obj.handle::Ptr{Cvoid})::ComplexF64
end

function set_bilBoundNiNi!(obj::FormulationConstants, val::ComplexF64)
	@ccall _HOFEM_LIB_PATH.formulationconstants_set_bilboundnini(obj.handle::Ptr{Cvoid}, val::ComplexF64)::Cvoid
end

function get_bilBoundNiNi(obj::FormulationConstants)::ComplexF64
	return @ccall _HOFEM_LIB_PATH.formulationconstants_get_bilboundnini(obj.handle::Ptr{Cvoid})::ComplexF64
end

function set_linNiFunction!(obj::FormulationConstants, val::ComplexF64)
	@ccall _HOFEM_LIB_PATH.formulationconstants_set_linnifunction(obj.handle::Ptr{Cvoid}, val::ComplexF64)::Cvoid
end

function get_linNiFunction(obj::FormulationConstants)::ComplexF64
	return @ccall _HOFEM_LIB_PATH.formulationconstants_get_linnifunction(obj.handle::Ptr{Cvoid})::ComplexF64
end

function set_bilNiGradFi!(obj::FormulationConstants, val::ComplexF64)
	@ccall _HOFEM_LIB_PATH.formulationconstants_set_bilnigradfi(obj.handle::Ptr{Cvoid}, val::ComplexF64)::Cvoid
end

function get_bilNiGradFi(obj::FormulationConstants)::ComplexF64
	return @ccall _HOFEM_LIB_PATH.formulationconstants_get_bilnigradfi(obj.handle::Ptr{Cvoid})::ComplexF64
end

function print_FormulationConstants(obj::FormulationConstants)
	@ccall _HOFEM_LIB_PATH.print_formulationconstants(obj.handle::Ptr{Cvoid})::Cvoid
end

struct FormulationTerms
	handle::Ptr{Cvoid}
end

function FormulationTerms()
	return FormulationTerms(@ccall _HOFEM_LIB_PATH.new_formulationterms()::Ptr{Cvoid})
end

function Base.finalize(obj::FormulationTerms)
	@ccall _HOFEM_LIB_PATH.free_formulationterms(obj.handle::Ptr{Cvoid})::Cvoid
end

function set_bilGradFiGradFi!(obj::FormulationTerms, val::Cuchar)
	@ccall _HOFEM_LIB_PATH.formulationterms_set_bilgradfigradfi(obj.handle::Ptr{Cvoid}, val::Cuchar)::Cvoid
end

function get_bilGradFiGradFi(obj::FormulationTerms)::Cuchar
	return @ccall _HOFEM_LIB_PATH.formulationterms_get_bilgradfigradfi(obj.handle::Ptr{Cvoid})::Cuchar
end

function set_bilFiFi!(obj::FormulationTerms, val::Cuchar)
	@ccall _HOFEM_LIB_PATH.formulationterms_set_bilfifi(obj.handle::Ptr{Cvoid}, val::Cuchar)::Cvoid
end

function get_bilFiFi(obj::FormulationTerms)::Cuchar
	return @ccall _HOFEM_LIB_PATH.formulationterms_get_bilfifi(obj.handle::Ptr{Cvoid})::Cuchar
end

function set_linFiFunction!(obj::FormulationTerms, val::Cuchar)
	@ccall _HOFEM_LIB_PATH.formulationterms_set_linfifunction(obj.handle::Ptr{Cvoid}, val::Cuchar)::Cvoid
end

function get_linFiFunction(obj::FormulationTerms)::Cuchar
	return @ccall _HOFEM_LIB_PATH.formulationterms_get_linfifunction(obj.handle::Ptr{Cvoid})::Cuchar
end

function set_linBoundFiFunction!(obj::FormulationTerms, val::Cuchar)
	@ccall _HOFEM_LIB_PATH.formulationterms_set_linboundfifunction(obj.handle::Ptr{Cvoid}, val::Cuchar)::Cvoid
end

function get_linBoundFiFunction(obj::FormulationTerms)::Cuchar
	return @ccall _HOFEM_LIB_PATH.formulationterms_get_linboundfifunction(obj.handle::Ptr{Cvoid})::Cuchar
end

function set_bilBoundFiFi!(obj::FormulationTerms, val::Cuchar)
	@ccall _HOFEM_LIB_PATH.formulationterms_set_bilboundfifi(obj.handle::Ptr{Cvoid}, val::Cuchar)::Cvoid
end

function get_bilBoundFiFi(obj::FormulationTerms)::Cuchar
	return @ccall _HOFEM_LIB_PATH.formulationterms_get_bilboundfifi(obj.handle::Ptr{Cvoid})::Cuchar
end

function set_bilRotNiRotNi!(obj::FormulationTerms, val::Cuchar)
	@ccall _HOFEM_LIB_PATH.formulationterms_set_bilrotnirotni(obj.handle::Ptr{Cvoid}, val::Cuchar)::Cvoid
end

function get_bilRotNiRotNi(obj::FormulationTerms)::Cuchar
	return @ccall _HOFEM_LIB_PATH.formulationterms_get_bilrotnirotni(obj.handle::Ptr{Cvoid})::Cuchar
end

function set_bilNiNi!(obj::FormulationTerms, val::Cuchar)
	@ccall _HOFEM_LIB_PATH.formulationterms_set_bilnini(obj.handle::Ptr{Cvoid}, val::Cuchar)::Cvoid
end

function get_bilNiNi(obj::FormulationTerms)::Cuchar
	return @ccall _HOFEM_LIB_PATH.formulationterms_get_bilnini(obj.handle::Ptr{Cvoid})::Cuchar
end

function set_linNiFunction!(obj::FormulationTerms, val::Cuchar)
	@ccall _HOFEM_LIB_PATH.formulationterms_set_linnifunction(obj.handle::Ptr{Cvoid}, val::Cuchar)::Cvoid
end

function get_linNiFunction(obj::FormulationTerms)::Cuchar
	return @ccall _HOFEM_LIB_PATH.formulationterms_get_linnifunction(obj.handle::Ptr{Cvoid})::Cuchar
end

function set_linBoundNiFunction!(obj::FormulationTerms, val::Cuchar)
	@ccall _HOFEM_LIB_PATH.formulationterms_set_linboundnifunction(obj.handle::Ptr{Cvoid}, val::Cuchar)::Cvoid
end

function get_linBoundNiFunction(obj::FormulationTerms)::Cuchar
	return @ccall _HOFEM_LIB_PATH.formulationterms_get_linboundnifunction(obj.handle::Ptr{Cvoid})::Cuchar
end

function set_bilBoundNiNi!(obj::FormulationTerms, val::Cuchar)
	@ccall _HOFEM_LIB_PATH.formulationterms_set_bilboundnini(obj.handle::Ptr{Cvoid}, val::Cuchar)::Cvoid
end

function get_bilBoundNiNi(obj::FormulationTerms)::Cuchar
	return @ccall _HOFEM_LIB_PATH.formulationterms_get_bilboundnini(obj.handle::Ptr{Cvoid})::Cuchar
end

function set_variational_unknown!(obj::FormulationTerms, val::String)
	@ccall _HOFEM_LIB_PATH.formulationterms_set_variational_unknown(obj.handle::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function get_variational_unknown(obj::FormulationTerms, val::String)
	@ccall _HOFEM_LIB_PATH.formulationterms_get_variational_unknown(obj.handle::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function set_basis_functions!(obj::FormulationTerms, val::String)
	@ccall _HOFEM_LIB_PATH.formulationterms_set_basis_functions(obj.handle::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function get_basis_functions(obj::FormulationTerms, val::String)
	@ccall _HOFEM_LIB_PATH.formulationterms_get_basis_functions(obj.handle::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function set_physics!(obj::FormulationTerms, val::String)
	@ccall _HOFEM_LIB_PATH.formulationterms_set_physics(obj.handle::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function get_physics(obj::FormulationTerms, val::String)
	@ccall _HOFEM_LIB_PATH.formulationterms_get_physics(obj.handle::Ptr{Cvoid}, val::Ptr{Cchar})::Cvoid
end

function print_FormulationTerms(obj::FormulationTerms)
	@ccall _HOFEM_LIB_PATH.print_formulationterms(obj.handle::Ptr{Cvoid})::Cvoid
end

function get_thermal_simulation()::thermal_formulation
	return thermal_formulation(@ccall _HOFEM_LIB_PATH.get_thermal_simulation()::Ptr{Cvoid})
end

function get_electromagnetic_simulation()::double_curl_wave_formulation
	return double_curl_wave_formulation(@ccall _HOFEM_LIB_PATH.get_electromagnetic_simulation()::Ptr{Cvoid})
end

function get_formulation_constants()::FormulationConstants
	return FormulationConstants(@ccall _HOFEM_LIB_PATH.get_formulation_constants()::Ptr{Cvoid})
end

function get_formulation_terms()::FormulationTerms
	return FormulationTerms(@ccall _HOFEM_LIB_PATH.get_formulation_terms()::Ptr{Cvoid})
end

function solving_mms_vectorial_equation()::Cuchar
	return @ccall _HOFEM_LIB_PATH.formulation_terms_module_mp_solving_mms_vectorial_equation_()::Cuchar
end

function solving_thermal_equation()::Cuchar
	return @ccall _HOFEM_LIB_PATH.formulation_terms_module_mp_solving_thermal_equation_()::Cuchar
end

function solving_wave_equation()::Cuchar
	return @ccall _HOFEM_LIB_PATH.formulation_terms_module_mp_solving_wave_equation_()::Cuchar
end

function using_nedelec_basis_functions()::Cuchar
	return @ccall _HOFEM_LIB_PATH.formulation_terms_module_mp_using_nedelec_basis_functions_()::Cuchar
end

function using_lagrange_basis_functions()::Cuchar
	return @ccall _HOFEM_LIB_PATH.formulation_terms_module_mp_using_lagrange_basis_functions_()::Cuchar
end

function solving_electric_field()::Cuchar
	return @ccall _HOFEM_LIB_PATH.formulation_terms_module_mp_solving_electric_field_()::Cuchar
end

function solving_magnetic_field()::Cuchar
	return @ccall _HOFEM_LIB_PATH.formulation_terms_module_mp_solving_magnetic_field_()::Cuchar
end

function is_step_time_strategy_uniform()::Cuchar
	return @ccall _HOFEM_LIB_PATH.formulation_terms_module_mp_is_step_time_strategy_uniform_()::Cuchar
end

function is_step_time_strategy_adaptive()::Cuchar
	return @ccall _HOFEM_LIB_PATH.formulation_terms_module_mp_is_step_time_strategy_adaptive_()::Cuchar
end


