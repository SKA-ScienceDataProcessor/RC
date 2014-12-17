module OskarCfg where
data ENUM_4 =
    OSKAR_BEAM_PATTERN_COORDS_UNDEF
  | OSKAR_BEAM_PATTERN_COORDS_BEAM_IMAGE
  | OSKAR_BEAM_PATTERN_COORDS_HEALPIX
  | OSKAR_BEAM_PATTERN_COORDS_SKY_MODEL
data ENUM_10 =
    OSKAR_BEAM_PATTERN_FRAME_UNDEF
  | OSKAR_BEAM_PATTERN_FRAME_EQUATORIAL
  | OSKAR_BEAM_PATTERN_FRAME_HORIZON
data ENUM_236 =
    OSKAR_IMAGE_DFT_2D
  | OSKAR_IMAGE_DFT_3D
  | OSKAR_IMAGE_FFT
data ENUM_241 =
    OSKAR_IMAGE_DIRECTION_OBSERVATION
  | OSKAR_IMAGE_DIRECTION_RA_DEC
data ENUM_252 =
    OSKAR_SYSTEM_NOISE_TELESCOPE_MODEL
  | OSKAR_SYSTEM_NOISE_OBS_SETTINGS
  | OSKAR_SYSTEM_NOISE_DATA_FILE
  | OSKAR_SYSTEM_NOISE_RANGE
  | OSKAR_SYSTEM_NOISE_RMS
  | OSKAR_SYSTEM_NOISE_SENSITIVITY
  | OSKAR_SYSTEM_NOISE_SYS_TEMP
  | OSKAR_SYSTEM_NOISE_NO_OVERRIDE
data ENUM_898 =
    OSKAR_MAP_UNITS_JY
  | OSKAR_MAP_UNITS_K_PER_SR
  | OSKAR_MAP_UNITS_MK_PER_SR
data OskarSettingsSkyExtendedSources = OskarSettingsSkyExtendedSources {
    osses_FWHM_major_rad :: Double
  , osses_FWHM_minor_rad :: Double
  , osses_position_angle_rad :: Double
  }
data OskarSettingsGaussianBeam = OskarSettingsGaussianBeam {
    osgb_fwhm_deg :: Double
  , osgb_ref_freq_hz :: Double
  }
data OskarSettingsInterferometer = OskarSettingsInterferometer {
    osi_channel_bandwidth_hz :: Double
  , osi_time_average_sec :: Double
  , osi_uv_filter_min :: Double
  , osi_uv_filter_max :: Double
  , osi_uv_filter_units :: Int
  , osi_num_vis_ave :: Int
  , osi_num_fringe_ave :: Int
  , osi_noise :: OskarSettingsSystemNoise
  , osi_oskar_vis_filename :: String
  , osi_ms_filename :: String
  , osi_use_common_sky :: Int
  , osi_scalar_mode :: Int
  }
data OskarSettingsBeamPattern = OskarSettingsBeamPattern {
    osbp_station_id :: Int
  , osbp_coord_grid_type :: Int
  , osbp_coord_frame_type :: Int
  , osbp_fov_deg :: (Double, Double)
  , osbp_size :: (Int, Int)
  , osbp_healpix_coord_type :: Int
  , osbp_nside :: Int
  , osbp_horizon_clip :: Int
  , osbp_sky_model :: String
  , osbp_output_beam_text_file :: String
  , osbp_oskar_image_voltage :: String
  , osbp_oskar_image_phase :: String
  , osbp_oskar_image_complex :: String
  , osbp_oskar_image_total_intensity :: String
  , osbp_fits_image_voltage :: String
  , osbp_fits_image_phase :: String
  , osbp_fits_image_total_intensity :: String
  }
data OskarSettingsSkyHealpixFits = OskarSettingsSkyHealpixFits {
    osshf_num_files :: Int
  , osshf_file :: [String]
  , osshf_coord_sys :: Int
  , osshf_map_units :: Int
  , osshf_filter :: OskarSettingsSkyFilter
  , osshf_extended_sources :: OskarSettingsSkyExtendedSources
  }
data OskarSettingsSkyPolarisation = OskarSettingsSkyPolarisation {
    ossp_mean_pol_fraction :: Double
  , ossp_std_pol_fraction :: Double
  , ossp_mean_pol_angle_rad :: Double
  , ossp_std_pol_angle_rad :: Double
  , ossp_seed :: Int
  }
data OskarSettingsTIDscreen = OskarSettingsTIDscreen {
    ostid_height_km :: Double
  , ostid_num_components :: Int
  , ostid_amp :: [Double]
  , ostid_wavelength :: [Double]
  , ostid_speed :: [Double]
  , ostid_theta :: [Double]
  }
data OskarSettingsPiercePoints = OskarSettingsPiercePoints {
    ospp_filename :: String
  }
data OskarSettingsElementPattern = OskarSettingsElementPattern {
    osep_enable_numerical_patterns :: Int
  , osep_functional_type :: Int
  , osep_dipole_length_units :: Int
  , osep_dipole_length :: Double
  , osep_taper :: OskarSettingsElementTaper
  }
data OskarSettingsSystemNoiseFreq = OskarSettingsSystemNoiseFreq {
    ossnf_specification :: Int
  , ossnf_file :: String
  , ossnf_number :: Int
  , ossnf_start :: Double
  , ossnf_inc :: Double
  }
data OskarSettingsSkyGeneratorGrid = OskarSettingsSkyGeneratorGrid {
    ossgg_extended_sources :: OskarSettingsSkyExtendedSources
  , ossgg_pol :: OskarSettingsSkyPolarisation
  , ossgg_side_length :: Int
  , ossgg_fov_rad :: Double
  , ossgg_mean_flux_jy :: Double
  , ossgg_std_flux_jy :: Double
  , ossgg_seed :: Int
  }
data OskarSettingsSkyFitsImage = OskarSettingsSkyFitsImage {
    ossfi_num_files :: Int
  , ossfi_file :: [String]
  , ossfi_spectral_index :: Double
  , ossfi_noise_floor :: Double
  , ossfi_min_peak_fraction :: Double
  , ossfi_downsample_factor :: Int
  }
data OskarSettingsIonosphere = OskarSettingsIonosphere {
    osi_enable :: Int
  , osi_min_elevation :: Double
  , osi_TEC0 :: Double
  , osi_num_TID_screens :: Int
  , osi_TID_files :: [String]
  , osi_TID :: [OskarSettingsTIDscreen]
  , osi_TECImage :: OskarSettingsTECImage
  , osi_pierce_points :: OskarSettingsPiercePoints
  }
data OskarSettingsSkyGeneratorRandomBrokenPowerLaw = OskarSettingsSkyGeneratorRandomBrokenPowerLaw {
    ossgrbpl_filter :: OskarSettingsSkyFilter
  , ossgrbpl_extended_sources :: OskarSettingsSkyExtendedSources
  , ossgrbpl_num_sources :: Int
  , ossgrbpl_flux_min :: Double
  , ossgrbpl_flux_max :: Double
  , ossgrbpl_threshold :: Double
  , ossgrbpl_power1 :: Double
  , ossgrbpl_power2 :: Double
  , ossgrbpl_seed :: Int
  }
data OskarSettingsSystemNoiseValue = OskarSettingsSystemNoiseValue {
    ossnv_specification :: Int
  , ossnv_rms :: OskarSettingsSystemNoiseType
  , ossnv_sensitivity :: OskarSettingsSystemNoiseType
  , ossnv_t_sys :: OskarSettingsSystemNoiseType
  , ossnv_area :: OskarSettingsSystemNoiseType
  , ossnv_efficiency :: OskarSettingsSystemNoiseType
  }
data OskarSettingsObservation = OskarSettingsObservation {
    oso_num_pointing_levels :: Int
  , oso_ra0_rad :: [Double]
  , oso_dec0_rad :: [Double]
  , oso_pointing_file :: String
  , oso_start_frequency_hz :: Double
  , oso_num_channels :: Int
  , oso_frequency_inc_hz :: Double
  , oso_num_time_steps :: Int
  , oso_start_mjd_utc :: Double
  , oso_length_seconds :: Double
  , oso_length_days :: Double
  , oso_dt_dump_days :: Double
  }
data OskarSettingsTelescope = OskarSettingsTelescope {
    ost_input_directory :: String
  , ost_output_directory :: String
  , ost_longitude_rad :: Double
  , ost_latitude_rad :: Double
  , ost_altitude_m :: Double
  , ost_station_type :: Int
  , ost_normalise_beams_at_phase_centre :: Int
  , ost_aperture_array :: OskarSettingsApertureArray
  , ost_gaussian_beam :: OskarSettingsGaussianBeam
  }
data OskarSettingsElementTaper = OskarSettingsElementTaper {
    oset_type :: Int
  , oset_cosine_power :: Double
  , oset_gaussian_fwhm_rad :: Double
  }
data OskarSettingsSystemNoiseType = OskarSettingsSystemNoiseType {
    ossnt_override :: Int
  , ossnt_file :: String
  , ossnt_start :: Double
  , ossnt_end :: Double
  }
data OskarSettingsSimulator = OskarSettingsSimulator {
    oss_double_precision :: Int
  , oss_max_sources_per_chunk :: Int
  , oss_num_cuda_devices :: Int
  , oss_keep_log_file :: Int
  , oss_cuda_device_ids :: [Int]
  }
data OskarSettingsSkyGenerator = OskarSettingsSkyGenerator {
    ossg_healpix :: OskarSettingsSkyGeneratorHealpix
  , ossg_grid :: OskarSettingsSkyGeneratorGrid
  , ossg_random_power_law :: OskarSettingsSkyGeneratorRandomPowerLaw
  , ossg_random_broken_power_law :: OskarSettingsSkyGeneratorRandomBrokenPowerLaw
  }
data OskarSettingsElementFit = OskarSettingsElementFit {
    osef_input_cst_file :: String
  , osef_output_directory :: String
  , osef_fits_image :: String
  , osef_pol_type :: Int
  , osef_element_type_index :: Int
  , osef_frequency_hz :: Double
  , osef_ignore_data_below_horizon :: Int
  , osef_ignore_data_at_pole :: Int
  , osef_average_fractional_error :: Double
  , osef_average_fractional_error_factor_increase :: Double
  }
data OskarSettingsSkyOskar = OskarSettingsSkyOskar {
    osso_num_files :: Int
  , osso_file :: [String]
  , osso_filter :: OskarSettingsSkyFilter
  , osso_extended_sources :: OskarSettingsSkyExtendedSources
  }
data OskarSettingsSkyFilter = OskarSettingsSkyFilter {
    ossf_flux_min :: Double
  , ossf_flux_max :: Double
  , ossf_radius_inner_rad :: Double
  , ossf_radius_outer_rad :: Double
  }
data OskarSettingsSkyGsm = OskarSettingsSkyGsm {
    ossg_file :: String
  , ossg_filter :: OskarSettingsSkyFilter
  , ossg_extended_sources :: OskarSettingsSkyExtendedSources
  }
data OskarSettingsApertureArray = OskarSettingsApertureArray {
    osaa_array_pattern :: OskarSettingsArrayPattern
  , osaa_element_pattern :: OskarSettingsElementPattern
  }
data OskarSettingsSystemNoise = OskarSettingsSystemNoise {
    ossn_enable :: Int
  , ossn_seed :: Int
  , ossn_freq :: OskarSettingsSystemNoiseFreq
  , ossn_value :: OskarSettingsSystemNoiseValue
  }
data OskarSettingsSky = OskarSettingsSky {
    oss_oskar_sky_model :: OskarSettingsSkyOskar
  , oss_gsm :: OskarSettingsSkyGsm
  , oss_fits_image :: OskarSettingsSkyFitsImage
  , oss_healpix_fits :: OskarSettingsSkyHealpixFits
  , oss_generator :: OskarSettingsSkyGenerator
  , oss_spectral_index :: OskarSettingsSkySpectralIndex
  , oss_output_text_file :: String
  , oss_output_binary_file :: String
  , oss_common_flux_filter_min_jy :: Double
  , oss_common_flux_filter_max_jy :: Double
  , oss_zero_failed_gaussians :: Int
  }
data OskarSettingsImage = OskarSettingsImage {
    osi_fov_deg :: Double
  , osi_size :: Int
  , osi_image_type :: Int
  , osi_channel_snapshots :: Int
  , osi_channel_range :: (Int, Int)
  , osi_time_snapshots :: Int
  , osi_time_range :: (Int, Int)
  , osi_transform_type :: Int
  , osi_input_vis_data :: String
  , osi_direction_type :: Int
  , osi_ra_deg :: Double
  , osi_dec_deg :: Double
  , osi_oskar_image :: String
  , osi_fits_image :: String
  }
data OskarSettingsSkyGeneratorRandomPowerLaw = OskarSettingsSkyGeneratorRandomPowerLaw {
    ossgrpl_filter :: OskarSettingsSkyFilter
  , ossgrpl_extended_sources :: OskarSettingsSkyExtendedSources
  , ossgrpl_num_sources :: Int
  , ossgrpl_flux_min :: Double
  , ossgrpl_flux_max :: Double
  , ossgrpl_power :: Double
  , ossgrpl_seed :: Int
  }
data OskarSettings = OskarSettings {
    os_settings_path :: String
  , os_sim :: OskarSettingsSimulator
  , os_sky :: OskarSettingsSky
  , os_obs :: OskarSettingsObservation
  , os_telescope :: OskarSettingsTelescope
  , os_element_fit :: OskarSettingsElementFit
  , os_interferometer :: OskarSettingsInterferometer
  , os_beam_pattern :: OskarSettingsBeamPattern
  , os_image :: OskarSettingsImage
  , os_ionosphere :: OskarSettingsIonosphere
  }
data OskarSettingsSkySpectralIndex = OskarSettingsSkySpectralIndex {
    osssi_override :: Int
  , osssi_ref_frequency_hz :: Double
  , osssi_mean :: Double
  , osssi_std_dev :: Double
  , osssi_seed :: Int
  }
data OskarSettingsArrayElement = OskarSettingsArrayElement {
    osae_apodisation_type :: Int
  , osae_gain :: Double
  , osae_gain_error_fixed :: Double
  , osae_gain_error_time :: Double
  , osae_phase_error_fixed_rad :: Double
  , osae_phase_error_time_rad :: Double
  , osae_position_error_xy_m :: Double
  , osae_x_orientation_error_rad :: Double
  , osae_y_orientation_error_rad :: Double
  , osae_seed_gain_errors :: Int
  , osae_seed_phase_errors :: Int
  , osae_seed_time_variable_errors :: Int
  , osae_seed_position_xy_errors :: Int
  , osae_seed_x_orientation_error :: Int
  , osae_seed_y_orientation_error :: Int
  }
data OskarSettingsSkyGeneratorHealpix = OskarSettingsSkyGeneratorHealpix {
    ossgh_filter :: OskarSettingsSkyFilter
  , ossgh_extended_sources :: OskarSettingsSkyExtendedSources
  , ossgh_nside :: Int
  , ossgh_amplitude :: Double
  }
data OskarSettingsTECImage = OskarSettingsTECImage {
    osteci_stationID :: Int
  , osteci_beam_centred :: Int
  , osteci_fov_rad :: Double
  , osteci_size :: Int
  , osteci_fits_file :: String
  , osteci_img_file :: String
  }
data OskarSettingsArrayPattern = OskarSettingsArrayPattern {
    osap_enable :: Int
  , osap_normalise :: Int
  , osap_element :: OskarSettingsArrayElement
  }