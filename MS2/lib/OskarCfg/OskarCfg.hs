{-# LANGUAGE
      TemplateHaskell
    , OverloadedStrings
  #-}

module OskarCfg where

import Text.Printf (printf)
import Data.Time (
    UTCTime(..)
  , fromGregorian
  , secondsToDiffTime
  , formatTime
  )
import System.Locale (defaultTimeLocale)

import OskarIniAutoGen

data OSKAR_BEAM_PATTERN_COORDS =
    OSKAR_BEAM_PATTERN_COORDS_UNDEF
  | OSKAR_BEAM_PATTERN_COORDS_BEAM_IMAGE
  | OSKAR_BEAM_PATTERN_COORDS_HEALPIX
  | OSKAR_BEAM_PATTERN_COORDS_SKY_MODEL
  deriving Enum
data OSKAR_BEAM_PATTERN_FRAME =
    OSKAR_BEAM_PATTERN_FRAME_UNDEF
  | OSKAR_BEAM_PATTERN_FRAME_EQUATORIAL
  | OSKAR_BEAM_PATTERN_FRAME_HORIZON
  deriving Enum
data OSKAR_IMAGE_FT =
    OSKAR_IMAGE_DFT_2D
  | OSKAR_IMAGE_DFT_3D
  | OSKAR_IMAGE_FFT
  deriving Enum
data OSKAR_IMAGE_DIRECTION =
    OSKAR_IMAGE_DIRECTION_OBSERVATION
  | OSKAR_IMAGE_DIRECTION_RA_DEC
  deriving Enum
data OSKAR_SYSTEM_NOISE =
    OSKAR_SYSTEM_NOISE_TELESCOPE_MODEL
  | OSKAR_SYSTEM_NOISE_OBS_SETTINGS
  | OSKAR_SYSTEM_NOISE_DATA_FILE
  | OSKAR_SYSTEM_NOISE_RANGE
  | OSKAR_SYSTEM_NOISE_RMS
  | OSKAR_SYSTEM_NOISE_SENSITIVITY
  | OSKAR_SYSTEM_NOISE_SYS_TEMP
  | OSKAR_SYSTEM_NOISE_NO_OVERRIDE
  deriving Enum
data OSKAR_MAP_UNITS =
    OSKAR_MAP_UNITS_JY
  | OSKAR_MAP_UNITS_K_PER_SR
  | OSKAR_MAP_UNITS_MK_PER_SR
  deriving Enum

$(gen_all
  [d|
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
      , osi_oskar_vis_filename :: Maybe OStr -- when Just also set image_output=true
      , osi_ms_filename :: Maybe String
      , osi_use_common_sky :: Maybe Int
      , osi_scalar_mode :: Maybe Int
      }
    instance NoAuto OskarSettingsInterferometer
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
      , osshf_file :: OList String
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
      , ostid_amp :: OList Double
      , ostid_wavelength :: OList Double
      , ostid_speed :: OList Double
      , ostid_theta :: OList Double
      }
    data OskarSettingsPiercePoints = OskarSettingsPiercePoints {
        ospp_filename :: String
      }
    data OskarSettingsElementPattern = OskarSettingsElementPattern {
        -- osep_enable_numerical_patterns :: Int
        osep_enable_numerical :: OBool
      -- , osep_functional_type :: Int
      , osep_functional_type :: OStr
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
      , ossfi_file :: OList String
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
      , osi_TID_files :: OList String
      -- , osi_TID :: OList OskarSettingsTIDscreen -- FIXME!
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
        -- oso_num_pointing_levels :: Int
      --   oso_ra0_rad :: OList Double
      -- , oso_dec0_rad :: OList Double
        oso_coords_deg :: [(Double, Double)]
      , oso_pointing_file :: Maybe String
      , oso_start_frequency_hz :: Double
      , oso_num_channels :: Int
      , oso_frequency_inc_hz :: Double
      , oso_num_time_steps :: Int
      -- , oso_start_mjd_utc :: Double
      , oso_start_utc :: UTCTime
      -- , oso_length_seconds :: Double
      -- , oso_length_days :: Double
      -- FIXME: move to DiffTime
      , oso_length :: UTCTime
      , oso_dt_dump_days :: Maybe Double
      }
    instance NoAuto OskarSettingsObservation
    data OskarSettingsTelescope = OskarSettingsTelescope {
        -- ost_input_directory :: String
        ost_input_directory :: OStr
      , ost_output_directory :: Maybe String
      -- , ost_longitude_rad :: Double
      , ost_longitude_deg :: Double
      -- , ost_latitude_rad :: Double
      , ost_latitude_deg :: Double
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
      , oss_cuda_device_ids :: OList Int
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
        -- osso_num_files :: Int
        -- osso_file :: OList String
        osso_file :: OList OStr
      , osso_filter :: Maybe OskarSettingsSkyFilter
      , osso_extended_sources :: Maybe OskarSettingsSkyExtendedSources
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
      -- , osi_image_type :: Int
      , osi_image_type :: OImageType
      , osi_root_path :: OStr -- Added Manually
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
        -- osap_enable :: Int
        osap_enable :: OBool
      -- , osap_normalise :: Maybe Int
      , osap_normalise :: Maybe OBool
      , osap_element :: Maybe OskarSettingsArrayElement
      }
    data OskarSettings = OskarSettings {
        -- os_settings_path :: String
        os_sim :: Maybe OskarSettingsSimulator
      , os_sky :: OskarSettingsSky
      , os_obs :: OskarSettingsObservation
      , os_telescope :: OskarSettingsTelescope
      , os_element_fit :: Maybe OskarSettingsElementFit
      , os_interferometer :: OskarSettingsInterferometer
      , os_beam_pattern :: Maybe OskarSettingsBeamPattern
      , os_image :: Maybe OskarSettingsImage
      , os_ionosphere :: Maybe OskarSettingsIonosphere
      }
    instance NoAuto OskarSettings
  |]
  )

-- We excluded these from auto instance generation by
-- making them instances of NoAuto class.
-- Thus we shall provide instances manually.
-- FIXME: Edit this according to Oskar logics.
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
                             pfx (strip_rec_uniq_prefix "osi_time_average_sec") s],
              \ pfx v
                -> case osi_uv_filter_min v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx (strip_rec_uniq_prefix "osi_uv_filter_min") s],
              \ pfx v
                -> case osi_uv_filter_max v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx (strip_rec_uniq_prefix "osi_uv_filter_max") s],
              \ pfx v
                -> case osi_uv_filter_units v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx (strip_rec_uniq_prefix "osi_uv_filter_units") s],
              \ pfx v
                -> case osi_num_vis_ave v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx (strip_rec_uniq_prefix "osi_num_vis_ave") s],
              \ pfx v
                -> case osi_num_fringe_ave v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx (strip_rec_uniq_prefix "osi_num_fringe_ave") s],
              \ pfx v
                -> case osi_noise v of
                     Nothing -> []
                     Just rr
                       -> showRecWithPrefix
                            ((scat pfx) $ (strip_rec_uniq_prefix "osi_noise")) rr,
              \ pfx v
                -> case osi_oskar_vis_filename v of
                     Nothing -> []
                     Just s
                       -> [show_immediate pfx "image_output" OTrue, -- manually edited!
                            show_immediate
                             pfx
                             (strip_rec_uniq_prefix "osi_oskar_vis_filename")
                             s],
              \ pfx v
                -> case osi_ms_filename v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx (strip_rec_uniq_prefix "osi_ms_filename") s],
              \ pfx v
                -> case osi_use_common_sky v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx (strip_rec_uniq_prefix "osi_use_common_sky") s],
              \ pfx v
                -> case osi_scalar_mode v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx (strip_rec_uniq_prefix "osi_scalar_mode") s]]
instance ShowRecWithPrefix OskarSettingsObservation where
  showRecWithPrefix
    = \ pfx v
        -> concatMap
             (\ f -> f pfx v)
             [
              {-
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
               -}
              \ pfx v ->
                    let
                      coords = oso_coords_deg v
                      ralist = map fst coords
                      declist = map snd coords
                    in [ show_immediate
                           pfx "phase_centre_ra_deg" (OList ralist)
                       , show_immediate
                           pfx "phase_centre_dec_deg" (OList declist)
                       ],
              \ pfx v
                -> case oso_pointing_file v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx (strip_rec_uniq_prefix "oso_pointing_file") s],
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
              {-
              \ pfx v
                -> [show_immediate
                      pfx
                      (strip_rec_uniq_prefix "oso_start_mjd_utc")
                      (oso_start_mjd_utc v)],
               -}
              \ pfx v
                -> [show_immediate
                      pfx
                      "start_time_utc"
                      (OStr $ formatTime defaultTimeLocale "%d-%m-%Y %T.%q" $ oso_start_utc v)],
              {-
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
               -}
              \ pfx v
                -> [show_immediate
                      pfx
                      "length"
                      (OStr $ formatTime defaultTimeLocale "%T.%q" $ oso_length v)],
              \ pfx v
                -> case oso_dt_dump_days v of
                     Nothing -> []
                     Just s
                       -> [show_immediate
                             pfx (strip_rec_uniq_prefix "oso_dt_dump_days") s]]


keyCvt :: ShowRecWithPrefix a => String -> a -> [String]
keyCvt key v = let skey = printf "[%s]" key in "" : skey : showRecWithPrefix "" v

mbKeyCvt :: ShowRecWithPrefix a => String -> Maybe a -> [String]
mbKeyCvt _ Nothing = []
mbKeyCvt key (Just v) = keyCvt key v

showSettings :: OskarSettings -> String
showSettings os = unlines $
     ["[General]", "version=2.5.1"]
  ++ mbKeyCvt "simulator"      (os_sim os)
  ++   keyCvt "sky"            (os_sky os)
  ++   keyCvt "observation"    (os_obs os)
  ++   keyCvt "telescope"      (os_telescope os)
  ++ mbKeyCvt "element_fit"    (os_element_fit os)
  ++   keyCvt "interferometer" (os_interferometer os)
  ++ mbKeyCvt "beam_pattern"   (os_beam_pattern os)
  ++ mbKeyCvt "image"          (os_image os)
  ++ mbKeyCvt "ionosphere"     (os_ionosphere os)

ska1low_template :: (OskarSettings, String)
ska1low_template = goWith (-65) 116.631289 (-26.697024)
  where
    goWith phase_centre_dec tele_lon tele_lat = (
        def {
          os_obs = def {
                     oso_coords_deg  = [(0, phase_centre_dec)]
                   }
        , os_telescope = def {
                           ost_longitude_deg = tele_lon
                         , ost_latitude_deg = tele_lat
                         , ost_aperture_array = def {
                                                  osaa_array_pattern = def {osap_enable = OFalse}
                                                , osaa_element_pattern = def {
                                                                           osep_enable_numerical = OFalse
                                                                         , osep_functional_type = "Geometric dipole"
                                                                         }
                                                }
                         }
        }
      , printf "0 %f 1 1" phase_centre_dec
      )

mk_ska1low_test_cfg :: OStr -> Double -> Int -> Int -> Integer -> OStr -> (OskarSettings, String, FilePath, FilePath)
mk_ska1low_test_cfg sky_model_fname start_freq num_chans num_timesteps length_t tele_layout_dir =
  let
    file_prefix = printf "%d-%d-%f" num_chans num_timesteps start_freq :: String
    vis_file_path = printf "%s.vis" file_prefix
    ini_file_path = printf "%s.ini" file_prefix
    precfg = fst ska1low_template
  in ( precfg {
         os_sky = (os_sky precfg) {
           oss_oskar_sky_model = def {osso_file = OList [sky_model_fname]}
         }
       , os_obs = (os_obs precfg) {
                    oso_start_frequency_hz = start_freq
                  , oso_num_channels = num_chans
                  , oso_num_time_steps = num_timesteps
                  , oso_start_utc = UTCTime day (secondsToDiffTime 0)
                  , oso_length = UTCTime day (secondsToDiffTime length_t)
                  }
       , os_telescope = (os_telescope precfg) {
                          ost_input_directory = tele_layout_dir
                        }
       , os_interferometer = (os_interferometer precfg) {osi_oskar_vis_filename = Just (OStr vis_file_path)}
       }
     , snd ska1low_template
     , vis_file_path
     , ini_file_path
     )
  where day = fromGregorian 2015 01 10

{- test
mkOut :: IO ()
mkOut =
  let
   (os, sky_model, vis_name, ini_name) = mk_ska1low_test_cfg "ska1low.sky" 100000000 1 72 (12*3600) "./telescope"
  in do
       writeFile "ska1low.sky" sky_model
       writeFile ini_name (showSettings os)
       putStrLn $ printf "Output filename is: %s" vis_name
 -}
