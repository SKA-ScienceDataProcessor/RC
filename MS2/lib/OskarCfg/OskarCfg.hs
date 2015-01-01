module OskarCfg where

import OskarIniAutoGen

data OSKAR_BEAM_PATTERN_COORDS =
    OSKAR_BEAM_PATTERN_COORDS_UNDEF
  | OSKAR_BEAM_PATTERN_COORDS_BEAM_IMAGE
  | OSKAR_BEAM_PATTERN_COORDS_HEALPIX
  | OSKAR_BEAM_PATTERN_COORDS_SKY_MODEL
data OSKAR_BEAM_PATTERN_FRAME =
    OSKAR_BEAM_PATTERN_FRAME_UNDEF
  | OSKAR_BEAM_PATTERN_FRAME_EQUATORIAL
  | OSKAR_BEAM_PATTERN_FRAME_HORIZON
data OSKAR_IMAGE_FT =
    OSKAR_IMAGE_DFT_2D
  | OSKAR_IMAGE_DFT_3D
  | OSKAR_IMAGE_FFT
data OSKAR_IMAGE_DIRECTION =
    OSKAR_IMAGE_DIRECTION_OBSERVATION
  | OSKAR_IMAGE_DIRECTION_RA_DEC
data OSKAR_SYSTEM_NOISE =
    OSKAR_SYSTEM_NOISE_TELESCOPE_MODEL
  | OSKAR_SYSTEM_NOISE_OBS_SETTINGS
  | OSKAR_SYSTEM_NOISE_DATA_FILE
  | OSKAR_SYSTEM_NOISE_RANGE
  | OSKAR_SYSTEM_NOISE_RMS
  | OSKAR_SYSTEM_NOISE_SENSITIVITY
  | OSKAR_SYSTEM_NOISE_SYS_TEMP
  | OSKAR_SYSTEM_NOISE_NO_OVERRIDE
data OSKAR_MAP_UNITS =
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
    osi_channel_bandwidth_hz :: Maybe Double
  , osi_time_average_sec :: Maybe Double
  , osi_uv_filter_min :: Maybe Double
  , osi_uv_filter_max :: Maybe Double
  , osi_uv_filter_units :: Maybe Int
  , osi_num_vis_ave :: Maybe Int
  , osi_num_fringe_ave :: Maybe Int
  , osi_noise :: Maybe OskarSettingsSystemNoise
  , osi_oskar_vis_filename :: Maybe String -- when Just also set image_output=true
  , osi_ms_filename :: Maybe String
  , osi_use_common_sky :: Maybe Int
  , osi_scalar_mode :: Maybe Int
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
  , osep_dipole_length_units :: Maybe Int
  , osep_dipole_length :: Maybe Double
  , osep_taper :: Maybe OskarSettingsElementTaper
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
  -- , osi_TID :: [OskarSettingsTIDscreen] -- FIXME!
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
  , oso_pointing_file :: Maybe String
  , oso_start_frequency_hz :: Double
  , oso_num_channels :: Int
  , oso_frequency_inc_hz :: Double
  , oso_num_time_steps :: Int
  , oso_start_mjd_utc :: Double
  , oso_length_seconds :: Double
  , oso_length_days :: Double
  , oso_dt_dump_days :: Maybe Double
  }
data OskarSettingsTelescope = OskarSettingsTelescope {
    ost_input_directory :: String
  , ost_output_directory :: Maybe String
  , ost_longitude_rad :: Double
  , ost_latitude_rad :: Double
  , ost_altitude_m :: Maybe Double
  , ost_station_type :: Maybe Int
  , ost_normalise_beams_at_phase_centre :: Maybe Int
  , ost_aperture_array :: OskarSettingsApertureArray
  , ost_gaussian_beam :: Maybe OskarSettingsGaussianBeam
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
  , oss_gsm :: Maybe OskarSettingsSkyGsm
  , oss_fits_image :: Maybe OskarSettingsSkyFitsImage
  , oss_healpix_fits :: Maybe OskarSettingsSkyHealpixFits
  , oss_generator :: Maybe OskarSettingsSkyGenerator
  , oss_spectral_index :: Maybe OskarSettingsSkySpectralIndex
  , oss_output_text_file :: Maybe String
  , oss_output_binary_file :: Maybe String
  , oss_common_flux_filter_min_jy :: Maybe Double
  , oss_common_flux_filter_max_jy :: Maybe Double
  , oss_zero_failed_gaussians :: Maybe Int
  }
data OskarSettingsImage = OskarSettingsImage {
    osi_fov_deg :: Maybe Double
  , osi_size :: Maybe Int
  , osi_image_type :: Int
  , osi_channel_snapshots :: Maybe Int
  , osi_channel_range :: Maybe (Int, Int)
  , osi_time_snapshots :: Maybe Int
  , osi_time_range :: Maybe (Int, Int)
  , osi_transform_type :: Maybe Int
  , osi_input_vis_data :: Maybe String
  , osi_direction_type :: Maybe Int
  , osi_ra_deg :: Maybe Double
  , osi_dec_deg :: Maybe Double
  , osi_oskar_image :: Maybe String
  , osi_fits_image :: Maybe String
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
  , osap_normalise :: Maybe Int
  , osap_element :: Maybe OskarSettingsArrayElement
  }
instance ShowRecWithPrefix OskarSettingsSkyExtendedSources where
  showRecWithPrefix
    = \ pfx v
        -> concatMap
             (\ f -> f pfx v)
             [\ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osses_FWHM_major_rad")
                      (osses_FWHM_major_rad v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osses_FWHM_minor_rad")
                      (osses_FWHM_minor_rad v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osses_position_angle_rad")
                      (osses_position_angle_rad v)]]
instance ShowRecWithPrefix OskarSettingsGaussianBeam where
  showRecWithPrefix
    = \ pfx v
        -> concatMap
             (\ f -> f pfx v)
             [\ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osgb_fwhm_deg")
                      (osgb_fwhm_deg v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osgb_ref_freq_hz")
                      (osgb_ref_freq_hz v)]]
instance ShowRecWithPrefix OskarSettingsInterferometer where
  showRecWithPrefix
    = \ pfx v
        -> concatMap
             (\ f -> f pfx v)
             [\ pfx v
                -> case osi_channel_bandwidth_hz v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix "osi_channel_bandwidth_hz")
                             s],
              \ pfx v
                -> case osi_time_average_sec v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix "osi_time_average_sec")
                             s],
              \ pfx v
                -> case osi_uv_filter_min v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix "osi_uv_filter_min")
                             s],
              \ pfx v
                -> case osi_uv_filter_max v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix "osi_uv_filter_max")
                             s],
              \ pfx v
                -> case osi_uv_filter_units v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix "osi_uv_filter_units")
                             s],
              \ pfx v
                -> case osi_num_vis_ave v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix "osi_num_vis_ave")
                             s],
              \ pfx v
                -> case osi_num_fringe_ave v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix "osi_num_fringe_ave")
                             s],
              \ pfx v
                -> case osi_noise v of
                     Nothing -> []
                     Just rr
                       -> showRecWithPrefix
                            ((scat pfx)
                             $ (strip_rec_uniq_prefix "osi_noise"))
                            rr,
              \ pfx v
                -> case osi_oskar_vis_filename v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix "osi_oskar_vis_filename")
                             s],
              \ pfx v
                -> case osi_ms_filename v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix "osi_ms_filename")
                             s],
              \ pfx v
                -> case osi_use_common_sky v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix "osi_use_common_sky")
                             s],
              \ pfx v
                -> case osi_scalar_mode v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix "osi_scalar_mode")
                             s]]
instance ShowRecWithPrefix OskarSettingsBeamPattern where
  showRecWithPrefix
    = \ pfx v
        -> concatMap
             (\ f -> f pfx v)
             [\ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osbp_station_id")
                      (osbp_station_id v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osbp_coord_grid_type")
                      (osbp_coord_grid_type v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osbp_coord_frame_type")
                      (osbp_coord_frame_type v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osbp_fov_deg")
                      (osbp_fov_deg v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osbp_size")
                      (osbp_size v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osbp_healpix_coord_type")
                      (osbp_healpix_coord_type v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osbp_nside")
                      (osbp_nside v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osbp_horizon_clip")
                      (osbp_horizon_clip v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osbp_sky_model")
                      (osbp_sky_model v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix
                         "osbp_output_beam_text_file")
                      (osbp_output_beam_text_file v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osbp_oskar_image_voltage")
                      (osbp_oskar_image_voltage v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osbp_oskar_image_phase")
                      (osbp_oskar_image_phase v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osbp_oskar_image_complex")
                      (osbp_oskar_image_complex v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix
                         "osbp_oskar_image_total_intensity")
                      (osbp_oskar_image_total_intensity v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osbp_fits_image_voltage")
                      (osbp_fits_image_voltage v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osbp_fits_image_phase")
                      (osbp_fits_image_phase v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix
                         "osbp_fits_image_total_intensity")
                      (osbp_fits_image_total_intensity v)]]
instance ShowRecWithPrefix OskarSettingsSkyHealpixFits where
  showRecWithPrefix
    = \ pfx v
        -> concatMap
             (\ f -> f pfx v)
             [\ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osshf_num_files")
                      (osshf_num_files v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osshf_file")
                      (osshf_file v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osshf_coord_sys")
                      (osshf_coord_sys v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osshf_map_units")
                      (osshf_map_units v)],
              \ pfx v
                -> showRecWithPrefix
                     ((scat pfx)
                      $ (strip_rec_uniq_prefix "osshf_filter"))
                     (osshf_filter v),
              \ pfx v
                -> showRecWithPrefix
                     ((scat pfx)
                      $ (strip_rec_uniq_prefix "osshf_extended_sources"))
                     (osshf_extended_sources v)]
instance ShowRecWithPrefix OskarSettingsSkyPolarisation where
  showRecWithPrefix
    = \ pfx v
        -> concatMap
             (\ f -> f pfx v)
             [\ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossp_mean_pol_fraction")
                      (ossp_mean_pol_fraction v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossp_std_pol_fraction")
                      (ossp_std_pol_fraction v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossp_mean_pol_angle_rad")
                      (ossp_mean_pol_angle_rad v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossp_std_pol_angle_rad")
                      (ossp_std_pol_angle_rad v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossp_seed")
                      (ossp_seed v)]]
instance ShowRecWithPrefix OskarSettingsTIDscreen where
  showRecWithPrefix
    = \ pfx v
        -> concatMap
             (\ f -> f pfx v)
             [\ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ostid_height_km")
                      (ostid_height_km v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ostid_num_components")
                      (ostid_num_components v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ostid_amp")
                      (ostid_amp v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ostid_wavelength")
                      (ostid_wavelength v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ostid_speed")
                      (ostid_speed v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ostid_theta")
                      (ostid_theta v)]]
instance ShowRecWithPrefix OskarSettingsPiercePoints where
  showRecWithPrefix
    = \ pfx v
        -> concatMap
             (\ f -> f pfx v)
             [\ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ospp_filename")
                      (ospp_filename v)]]
instance ShowRecWithPrefix OskarSettingsElementPattern where
  showRecWithPrefix
    = \ pfx v
        -> concatMap
             (\ f -> f pfx v)
             [\ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix
                         "osep_enable_numerical_patterns")
                      (osep_enable_numerical_patterns v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osep_functional_type")
                      (osep_functional_type v)],
              \ pfx v
                -> case osep_dipole_length_units v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix "osep_dipole_length_units")
                             s],
              \ pfx v
                -> case osep_dipole_length v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix "osep_dipole_length")
                             s],
              \ pfx v
                -> case osep_taper v of
                     Nothing -> []
                     Just rr
                       -> showRecWithPrefix
                            ((scat pfx)
                             $ (strip_rec_uniq_prefix "osep_taper"))
                            rr]
instance ShowRecWithPrefix OskarSettingsSystemNoiseFreq where
  showRecWithPrefix
    = \ pfx v
        -> concatMap
             (\ f -> f pfx v)
             [\ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossnf_specification")
                      (ossnf_specification v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossnf_file")
                      (ossnf_file v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossnf_number")
                      (ossnf_number v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossnf_start")
                      (ossnf_start v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossnf_inc")
                      (ossnf_inc v)]]
instance ShowRecWithPrefix OskarSettingsSkyGeneratorGrid where
  showRecWithPrefix
    = \ pfx v
        -> concatMap
             (\ f -> f pfx v)
             [\ pfx v
                -> showRecWithPrefix
                     ((scat pfx)
                      $ (strip_rec_uniq_prefix "ossgg_extended_sources"))
                     (ossgg_extended_sources v),
              \ pfx v
                -> showRecWithPrefix
                     ((scat pfx)
                      $ (strip_rec_uniq_prefix "ossgg_pol"))
                     (ossgg_pol v),
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossgg_side_length")
                      (ossgg_side_length v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossgg_fov_rad")
                      (ossgg_fov_rad v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossgg_mean_flux_jy")
                      (ossgg_mean_flux_jy v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossgg_std_flux_jy")
                      (ossgg_std_flux_jy v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossgg_seed")
                      (ossgg_seed v)]]
instance ShowRecWithPrefix OskarSettingsSkyFitsImage where
  showRecWithPrefix
    = \ pfx v
        -> concatMap
             (\ f -> f pfx v)
             [\ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossfi_num_files")
                      (ossfi_num_files v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossfi_file")
                      (ossfi_file v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossfi_spectral_index")
                      (ossfi_spectral_index v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossfi_noise_floor")
                      (ossfi_noise_floor v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossfi_min_peak_fraction")
                      (ossfi_min_peak_fraction v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossfi_downsample_factor")
                      (ossfi_downsample_factor v)]]
instance ShowRecWithPrefix OskarSettingsIonosphere where
  showRecWithPrefix
    = \ pfx v
        -> concatMap
             (\ f -> f pfx v)
             [\ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osi_enable")
                      (osi_enable v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osi_min_elevation")
                      (osi_min_elevation v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osi_TEC0")
                      (osi_TEC0 v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osi_num_TID_screens")
                      (osi_num_TID_screens v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osi_TID_files")
                      (osi_TID_files v)],
              \ pfx v
                -> showRecWithPrefix
                     ((scat pfx)
                      $ (strip_rec_uniq_prefix "osi_TECImage"))
                     (osi_TECImage v),
              \ pfx v
                -> showRecWithPrefix
                     ((scat pfx)
                      $ (strip_rec_uniq_prefix "osi_pierce_points"))
                     (osi_pierce_points v)]
instance ShowRecWithPrefix OskarSettingsSkyGeneratorRandomBrokenPowerLaw where
  showRecWithPrefix
    = \ pfx v
        -> concatMap
             (\ f -> f pfx v)
             [\ pfx v
                -> showRecWithPrefix
                     ((scat pfx)
                      $ (strip_rec_uniq_prefix "ossgrbpl_filter"))
                     (ossgrbpl_filter v),
              \ pfx v
                -> showRecWithPrefix
                     ((scat pfx)
                      $ (strip_rec_uniq_prefix
                           "ossgrbpl_extended_sources"))
                     (ossgrbpl_extended_sources v),
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossgrbpl_num_sources")
                      (ossgrbpl_num_sources v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossgrbpl_flux_min")
                      (ossgrbpl_flux_min v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossgrbpl_flux_max")
                      (ossgrbpl_flux_max v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossgrbpl_threshold")
                      (ossgrbpl_threshold v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossgrbpl_power1")
                      (ossgrbpl_power1 v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossgrbpl_power2")
                      (ossgrbpl_power2 v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossgrbpl_seed")
                      (ossgrbpl_seed v)]]
instance ShowRecWithPrefix OskarSettingsSystemNoiseValue where
  showRecWithPrefix
    = \ pfx v
        -> concatMap
             (\ f -> f pfx v)
             [\ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossnv_specification")
                      (ossnv_specification v)],
              \ pfx v
                -> showRecWithPrefix
                     ((scat pfx)
                      $ (strip_rec_uniq_prefix "ossnv_rms"))
                     (ossnv_rms v),
              \ pfx v
                -> showRecWithPrefix
                     ((scat pfx)
                      $ (strip_rec_uniq_prefix "ossnv_sensitivity"))
                     (ossnv_sensitivity v),
              \ pfx v
                -> showRecWithPrefix
                     ((scat pfx)
                      $ (strip_rec_uniq_prefix "ossnv_t_sys"))
                     (ossnv_t_sys v),
              \ pfx v
                -> showRecWithPrefix
                     ((scat pfx)
                      $ (strip_rec_uniq_prefix "ossnv_area"))
                     (ossnv_area v),
              \ pfx v
                -> showRecWithPrefix
                     ((scat pfx)
                      $ (strip_rec_uniq_prefix "ossnv_efficiency"))
                     (ossnv_efficiency v)]
instance ShowRecWithPrefix OskarSettingsObservation where
  showRecWithPrefix
    = \ pfx v
        -> concatMap
             (\ f -> f pfx v)
             [\ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "oso_num_pointing_levels")
                      (oso_num_pointing_levels v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "oso_ra0_rad")
                      (oso_ra0_rad v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "oso_dec0_rad")
                      (oso_dec0_rad v)],
              \ pfx v
                -> case oso_pointing_file v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix "oso_pointing_file")
                             s],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "oso_start_frequency_hz")
                      (oso_start_frequency_hz v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "oso_num_channels")
                      (oso_num_channels v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "oso_frequency_inc_hz")
                      (oso_frequency_inc_hz v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "oso_num_time_steps")
                      (oso_num_time_steps v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "oso_start_mjd_utc")
                      (oso_start_mjd_utc v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "oso_length_seconds")
                      (oso_length_seconds v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "oso_length_days")
                      (oso_length_days v)],
              \ pfx v
                -> case oso_dt_dump_days v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix "oso_dt_dump_days")
                             s]]
instance ShowRecWithPrefix OskarSettingsTelescope where
  showRecWithPrefix
    = \ pfx v
        -> concatMap
             (\ f -> f pfx v)
             [\ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ost_input_directory")
                      (ost_input_directory v)],
              \ pfx v
                -> case ost_output_directory v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix "ost_output_directory")
                             s],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ost_longitude_rad")
                      (ost_longitude_rad v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ost_latitude_rad")
                      (ost_latitude_rad v)],
              \ pfx v
                -> case ost_altitude_m v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix "ost_altitude_m")
                             s],
              \ pfx v
                -> case ost_station_type v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix "ost_station_type")
                             s],
              \ pfx v
                -> case ost_normalise_beams_at_phase_centre v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix
                                "ost_normalise_beams_at_phase_centre")
                             s],
              \ pfx v
                -> showRecWithPrefix
                     ((scat pfx)
                      $ (strip_rec_uniq_prefix "ost_aperture_array"))
                     (ost_aperture_array v),
              \ pfx v
                -> case ost_gaussian_beam v of
                     Nothing -> []
                     Just rr
                       -> showRecWithPrefix
                            ((scat pfx)
                             $ (strip_rec_uniq_prefix "ost_gaussian_beam"))
                            rr]
instance ShowRecWithPrefix OskarSettingsElementTaper where
  showRecWithPrefix
    = \ pfx v
        -> concatMap
             (\ f -> f pfx v)
             [\ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "oset_type")
                      (oset_type v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "oset_cosine_power")
                      (oset_cosine_power v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "oset_gaussian_fwhm_rad")
                      (oset_gaussian_fwhm_rad v)]]
instance ShowRecWithPrefix OskarSettingsSystemNoiseType where
  showRecWithPrefix
    = \ pfx v
        -> concatMap
             (\ f -> f pfx v)
             [\ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossnt_override")
                      (ossnt_override v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossnt_file")
                      (ossnt_file v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossnt_start")
                      (ossnt_start v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossnt_end")
                      (ossnt_end v)]]
instance ShowRecWithPrefix OskarSettingsSimulator where
  showRecWithPrefix
    = \ pfx v
        -> concatMap
             (\ f -> f pfx v)
             [\ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "oss_double_precision")
                      (oss_double_precision v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "oss_max_sources_per_chunk")
                      (oss_max_sources_per_chunk v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "oss_num_cuda_devices")
                      (oss_num_cuda_devices v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "oss_keep_log_file")
                      (oss_keep_log_file v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "oss_cuda_device_ids")
                      (oss_cuda_device_ids v)]]
instance ShowRecWithPrefix OskarSettingsSkyGenerator where
  showRecWithPrefix
    = \ pfx v
        -> concatMap
             (\ f -> f pfx v)
             [\ pfx v
                -> showRecWithPrefix
                     ((scat pfx)
                      $ (strip_rec_uniq_prefix "ossg_healpix"))
                     (ossg_healpix v),
              \ pfx v
                -> showRecWithPrefix
                     ((scat pfx)
                      $ (strip_rec_uniq_prefix "ossg_grid"))
                     (ossg_grid v),
              \ pfx v
                -> showRecWithPrefix
                     ((scat pfx)
                      $ (strip_rec_uniq_prefix "ossg_random_power_law"))
                     (ossg_random_power_law v),
              \ pfx v
                -> showRecWithPrefix
                     ((scat pfx)
                      $ (strip_rec_uniq_prefix
                           "ossg_random_broken_power_law"))
                     (ossg_random_broken_power_law v)]
instance ShowRecWithPrefix OskarSettingsElementFit where
  showRecWithPrefix
    = \ pfx v
        -> concatMap
             (\ f -> f pfx v)
             [\ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osef_input_cst_file")
                      (osef_input_cst_file v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osef_output_directory")
                      (osef_output_directory v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osef_fits_image")
                      (osef_fits_image v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osef_pol_type")
                      (osef_pol_type v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osef_element_type_index")
                      (osef_element_type_index v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osef_frequency_hz")
                      (osef_frequency_hz v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix
                         "osef_ignore_data_below_horizon")
                      (osef_ignore_data_below_horizon v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osef_ignore_data_at_pole")
                      (osef_ignore_data_at_pole v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix
                         "osef_average_fractional_error")
                      (osef_average_fractional_error v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix
                         "osef_average_fractional_error_factor_increase")
                      (osef_average_fractional_error_factor_increase v)]]
instance ShowRecWithPrefix OskarSettingsSkyOskar where
  showRecWithPrefix
    = \ pfx v
        -> concatMap
             (\ f -> f pfx v)
             [\ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osso_num_files")
                      (osso_num_files v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osso_file")
                      (osso_file v)],
              \ pfx v
                -> showRecWithPrefix
                     ((scat pfx)
                      $ (strip_rec_uniq_prefix "osso_filter"))
                     (osso_filter v),
              \ pfx v
                -> showRecWithPrefix
                     ((scat pfx)
                      $ (strip_rec_uniq_prefix "osso_extended_sources"))
                     (osso_extended_sources v)]
instance ShowRecWithPrefix OskarSettingsSkyFilter where
  showRecWithPrefix
    = \ pfx v
        -> concatMap
             (\ f -> f pfx v)
             [\ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossf_flux_min")
                      (ossf_flux_min v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossf_flux_max")
                      (ossf_flux_max v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossf_radius_inner_rad")
                      (ossf_radius_inner_rad v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossf_radius_outer_rad")
                      (ossf_radius_outer_rad v)]]
instance ShowRecWithPrefix OskarSettingsSkyGsm where
  showRecWithPrefix
    = \ pfx v
        -> concatMap
             (\ f -> f pfx v)
             [\ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossg_file")
                      (ossg_file v)],
              \ pfx v
                -> showRecWithPrefix
                     ((scat pfx)
                      $ (strip_rec_uniq_prefix "ossg_filter"))
                     (ossg_filter v),
              \ pfx v
                -> showRecWithPrefix
                     ((scat pfx)
                      $ (strip_rec_uniq_prefix "ossg_extended_sources"))
                     (ossg_extended_sources v)]
instance ShowRecWithPrefix OskarSettingsApertureArray where
  showRecWithPrefix
    = \ pfx v
        -> concatMap
             (\ f -> f pfx v)
             [\ pfx v
                -> showRecWithPrefix
                     ((scat pfx)
                      $ (strip_rec_uniq_prefix "osaa_array_pattern"))
                     (osaa_array_pattern v),
              \ pfx v
                -> showRecWithPrefix
                     ((scat pfx)
                      $ (strip_rec_uniq_prefix "osaa_element_pattern"))
                     (osaa_element_pattern v)]
instance ShowRecWithPrefix OskarSettingsSystemNoise where
  showRecWithPrefix
    = \ pfx v
        -> concatMap
             (\ f -> f pfx v)
             [\ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossn_enable")
                      (ossn_enable v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossn_seed")
                      (ossn_seed v)],
              \ pfx v
                -> showRecWithPrefix
                     ((scat pfx)
                      $ (strip_rec_uniq_prefix "ossn_freq"))
                     (ossn_freq v),
              \ pfx v
                -> showRecWithPrefix
                     ((scat pfx)
                      $ (strip_rec_uniq_prefix "ossn_value"))
                     (ossn_value v)]
instance ShowRecWithPrefix OskarSettingsSky where
  showRecWithPrefix
    = \ pfx v
        -> concatMap
             (\ f -> f pfx v)
             [\ pfx v
                -> showRecWithPrefix
                     ((scat pfx)
                      $ (strip_rec_uniq_prefix "oss_oskar_sky_model"))
                     (oss_oskar_sky_model v),
              \ pfx v
                -> case oss_gsm v of
                     Nothing -> []
                     Just rr
                       -> showRecWithPrefix
                            ((scat pfx)
                             $ (strip_rec_uniq_prefix "oss_gsm"))
                            rr,
              \ pfx v
                -> case oss_fits_image v of
                     Nothing -> []
                     Just rr
                       -> showRecWithPrefix
                            ((scat pfx)
                             $ (strip_rec_uniq_prefix "oss_fits_image"))
                            rr,
              \ pfx v
                -> case oss_healpix_fits v of
                     Nothing -> []
                     Just rr
                       -> showRecWithPrefix
                            ((scat pfx)
                             $ (strip_rec_uniq_prefix "oss_healpix_fits"))
                            rr,
              \ pfx v
                -> case oss_generator v of
                     Nothing -> []
                     Just rr
                       -> showRecWithPrefix
                            ((scat pfx)
                             $ (strip_rec_uniq_prefix "oss_generator"))
                            rr,
              \ pfx v
                -> case oss_spectral_index v of
                     Nothing -> []
                     Just rr
                       -> showRecWithPrefix
                            ((scat pfx)
                             $ (strip_rec_uniq_prefix "oss_spectral_index"))
                            rr,
              \ pfx v
                -> case oss_output_text_file v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix "oss_output_text_file")
                             s],
              \ pfx v
                -> case oss_output_binary_file v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix "oss_output_binary_file")
                             s],
              \ pfx v
                -> case oss_common_flux_filter_min_jy v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix
                                "oss_common_flux_filter_min_jy")
                             s],
              \ pfx v
                -> case oss_common_flux_filter_max_jy v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix
                                "oss_common_flux_filter_max_jy")
                             s],
              \ pfx v
                -> case oss_zero_failed_gaussians v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix "oss_zero_failed_gaussians")
                             s]]
instance ShowRecWithPrefix OskarSettingsImage where
  showRecWithPrefix
    = \ pfx v
        -> concatMap
             (\ f -> f pfx v)
             [\ pfx v
                -> case osi_fov_deg v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix "osi_fov_deg")
                             s],
              \ pfx v
                -> case osi_size v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix "osi_size")
                             s],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osi_image_type")
                      (osi_image_type v)],
              \ pfx v
                -> case osi_channel_snapshots v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix "osi_channel_snapshots")
                             s],
              \ pfx v
                -> case osi_channel_range v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix "osi_channel_range")
                             s],
              \ pfx v
                -> case osi_time_snapshots v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix "osi_time_snapshots")
                             s],
              \ pfx v
                -> case osi_time_range v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix "osi_time_range")
                             s],
              \ pfx v
                -> case osi_transform_type v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix "osi_transform_type")
                             s],
              \ pfx v
                -> case osi_input_vis_data v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix "osi_input_vis_data")
                             s],
              \ pfx v
                -> case osi_direction_type v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix "osi_direction_type")
                             s],
              \ pfx v
                -> case osi_ra_deg v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix "osi_ra_deg")
                             s],
              \ pfx v
                -> case osi_dec_deg v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix "osi_dec_deg")
                             s],
              \ pfx v
                -> case osi_oskar_image v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix "osi_oskar_image")
                             s],
              \ pfx v
                -> case osi_fits_image v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix "osi_fits_image")
                             s]]
instance ShowRecWithPrefix OskarSettingsSkyGeneratorRandomPowerLaw where
  showRecWithPrefix
    = \ pfx v
        -> concatMap
             (\ f -> f pfx v)
             [\ pfx v
                -> showRecWithPrefix
                     ((scat pfx)
                      $ (strip_rec_uniq_prefix "ossgrpl_filter"))
                     (ossgrpl_filter v),
              \ pfx v
                -> showRecWithPrefix
                     ((scat pfx)
                      $ (strip_rec_uniq_prefix
                           "ossgrpl_extended_sources"))
                     (ossgrpl_extended_sources v),
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossgrpl_num_sources")
                      (ossgrpl_num_sources v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossgrpl_flux_min")
                      (ossgrpl_flux_min v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossgrpl_flux_max")
                      (ossgrpl_flux_max v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossgrpl_power")
                      (ossgrpl_power v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossgrpl_seed")
                      (ossgrpl_seed v)]]
instance ShowRecWithPrefix OskarSettingsSkySpectralIndex where
  showRecWithPrefix
    = \ pfx v
        -> concatMap
             (\ f -> f pfx v)
             [\ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osssi_override")
                      (osssi_override v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osssi_ref_frequency_hz")
                      (osssi_ref_frequency_hz v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osssi_mean")
                      (osssi_mean v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osssi_std_dev")
                      (osssi_std_dev v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osssi_seed")
                      (osssi_seed v)]]
instance ShowRecWithPrefix OskarSettingsArrayElement where
  showRecWithPrefix
    = \ pfx v
        -> concatMap
             (\ f -> f pfx v)
             [\ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osae_apodisation_type")
                      (osae_apodisation_type v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osae_gain")
                      (osae_gain v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osae_gain_error_fixed")
                      (osae_gain_error_fixed v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osae_gain_error_time")
                      (osae_gain_error_time v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix
                         "osae_phase_error_fixed_rad")
                      (osae_phase_error_fixed_rad v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osae_phase_error_time_rad")
                      (osae_phase_error_time_rad v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osae_position_error_xy_m")
                      (osae_position_error_xy_m v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix
                         "osae_x_orientation_error_rad")
                      (osae_x_orientation_error_rad v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix
                         "osae_y_orientation_error_rad")
                      (osae_y_orientation_error_rad v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osae_seed_gain_errors")
                      (osae_seed_gain_errors v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osae_seed_phase_errors")
                      (osae_seed_phase_errors v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix
                         "osae_seed_time_variable_errors")
                      (osae_seed_time_variable_errors v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix
                         "osae_seed_position_xy_errors")
                      (osae_seed_position_xy_errors v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix
                         "osae_seed_x_orientation_error")
                      (osae_seed_x_orientation_error v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix
                         "osae_seed_y_orientation_error")
                      (osae_seed_y_orientation_error v)]]
instance ShowRecWithPrefix OskarSettingsSkyGeneratorHealpix where
  showRecWithPrefix
    = \ pfx v
        -> concatMap
             (\ f -> f pfx v)
             [\ pfx v
                -> showRecWithPrefix
                     ((scat pfx)
                      $ (strip_rec_uniq_prefix "ossgh_filter"))
                     (ossgh_filter v),
              \ pfx v
                -> showRecWithPrefix
                     ((scat pfx)
                      $ (strip_rec_uniq_prefix "ossgh_extended_sources"))
                     (ossgh_extended_sources v),
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossgh_nside")
                      (ossgh_nside v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "ossgh_amplitude")
                      (ossgh_amplitude v)]]
instance ShowRecWithPrefix OskarSettingsTECImage where
  showRecWithPrefix
    = \ pfx v
        -> concatMap
             (\ f -> f pfx v)
             [\ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osteci_stationID")
                      (osteci_stationID v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osteci_beam_centred")
                      (osteci_beam_centred v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osteci_fov_rad")
                      (osteci_fov_rad v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osteci_size")
                      (osteci_size v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osteci_fits_file")
                      (osteci_fits_file v)],
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osteci_img_file")
                      (osteci_img_file v)]]
instance ShowRecWithPrefix OskarSettingsArrayPattern where
  showRecWithPrefix
    = \ pfx v
        -> concatMap
             (\ f -> f pfx v)
             [\ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "osap_enable")
                      (osap_enable v)],
              \ pfx v
                -> case osap_normalise v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx
                             (strip_rec_uniq_prefix "osap_normalise")
                             s],
              \ pfx v
                -> case osap_element v of
                     Nothing -> []
                     Just rr
                       -> showRecWithPrefix
                            ((scat pfx)
                             $ (strip_rec_uniq_prefix "osap_element"))
                            rr]

-- It is shown differently
data OskarSettings = OskarSettings {
    os_settings_path :: String
  , os_sim :: Maybe OskarSettingsSimulator
  , os_sky :: OskarSettingsSky
  , os_obs :: OskarSettingsObservation
  , os_telescope :: OskarSettingsTelescope
  , os_element_fit :: Maybe OskarSettingsElementFit
  , os_interferometer :: OskarSettingsInterferometer
  , os_beam_pattern :: Maybe OskarSettingsBeamPattern
  , os_image :: OskarSettingsImage
  , os_ionosphere :: Maybe OskarSettingsIonosphere
  }

test :: [String]
test = showRecWithPrefix "" (OskarSettingsElementPattern 5 6 Nothing (Just 1.1) (Just $ OskarSettingsElementTaper 5 5.5 6.7))
