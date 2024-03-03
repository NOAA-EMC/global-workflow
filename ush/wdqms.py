import sys
import os
import logging
import pandas as pd
import numpy as np
from netCDF4 import Dataset
from pathlib import Path


class WDQMS:

    def __init__(self, inputfiles, wdqms_type, outdir, 
                 loglvl=logging.INFO):

        # Start logging
        logging.basicConfig(filename='file.log',
                            filemode='w',
                            level=loglvl,
                            format='%(levelname)s:%(message)s')
        
        self.wdqms_type = wdqms_type
        self.outdir = outdir

        logging.info("Working in wdqms()")

        # Create dataframes from GSI diag files
        logging.info('Creating dataframe from GSI diag files ...')

        self.wdqms_type_dict = {
            'SYNOP': {
                'df_type': self._create_conv_df,
                'obs_types': [181, 187, 281, 287],
                'variable_ids': {'ps': 110, 'q': 58, 't': 39, 'u': 41, 'v': 42}
            },
            'TEMP': {
                'df_type': self._create_sondes_df,
                'obs_types': [120, 220],
                'variable_ids': {'ps': 110, 'q': 29, 't': 2, 'u': 3, 'v': 4}
            },
            'MARINE': {
                'df_type': self._create_conv_df,
                'obs_types': [180, 183, 280, 282, 284],
                'variable_ids': {'ps': 110, 'q': 58, 't': 39, 'u': 41, 'v': 42}
            }
        }

        df_list = []

        for file in inputfiles:
            logging.info(f'Working on {file} ...')
            logging.info(f'Reading gsi diag ... ')

            df = self._read_gsi_diag(file)
            df_list.append(df)

        df_total = pd.concat(df_list)
        
        logging.info('Files successfully read into dataframe!')

        # Grab data specific to WDQMS type
        df_total = self._wdqms_type_requirements(df_total)

        # Grab actual datetimes from datetime + timedelta
        df_total = self._get_datetimes(df_total)

        # Adjust relative humidity data
        df_total = self._genqsat(df_total)

        # Add Status Flag column
        df_total = self._create_status_flag(df_total)
        df_total['StatusFlag'] = df_total['StatusFlag'].astype(int)

        # Sort by Station ID
        df_total['Station_ID'] = df_total['Station_ID'].astype(str)
        df_total = df_total.sort_values('Station_ID')

        logging.info(f'Creating dataframe for {self.wdqms_type} type ...')

        # Create dataframe for appropriate WDQMS type
        output_df = self.wdqms_type_dict[self.wdqms_type]['df_type'](df_total)

        # Get str datetime
        self.datetime = inputfiles[0].split('/')[-1].split('.')[-2]

        out_filename = self._df_to_csv(output_df)

        logging.info(f"Success! Output file saved to: {out_filename}")
        logging.info("Exiting ...")
        sys.exit()

        return

    def _wdqms_type_requirements(self, df):
        """
        Filter dataframe to only include specific observation types.
        """
        logging.info("Working in wdqms_type_requirements()")
        logging.debug(f"WDQMS Type: {self.wdqms_type}")
        logging.debug(f"Total observations for {self.wdqms_type} before filter: {len(df)}")

        obs_types = self.wdqms_type_dict[self.wdqms_type]['obs_types']

        df = df.loc[(df['Observation_Type'].isin(obs_types))]

        if self.wdqms_type in ['SYNOP', 'MARINE']:
            # Only include -3 > val >= 3 to avoid overlapping in cycles
            df = df.loc[df['Time'] != -3.]
            
            # Remove bad background departures for each variable
            df.loc[(df['var_id'] == 110) & (df['Obs_Minus_Forecast_adjusted'].abs() > 200),
                   'Obs_Minus_Forecast_adjusted'] = -999.9999
            df.loc[(df['var_id'] != 110) & (df['Obs_Minus_Forecast_adjusted'].abs() > 500),
                   'Obs_Minus_Forecast_adjusted'] = -999.9999

        logging.debug(f"Total observations for {self.wdqms_type} after filter: {len(df)}")
        logging.info("Exiting wdqms_type_requirements()")

        return df

    def _get_datetimes(self, df):
        """
        Use 'Datetime' and 'Time' columns to create new datetime and
        separate into new columns: 'YYYYMMDD' and 'HHMMSS'
        Args:
            df : (df) pandas dataframe populated with data from GSI
                 diagnostic files
        Returns:
            df: (df) the same dataframe read in with new columns:
                'YYYYMMDD' and 'HHMMSS'
        """
        logging.info("Working in get_datetimes()")

        # Convert 'Datetime' column from str to datetime
        dates = pd.to_datetime(df['Datetime'], format='%Y%m%d%H')
        # Converts 'Time' column to time delta in hours
        hrs = pd.to_timedelta(df['Time'], unit='hours')
        # Actual datetime of ob adding datetime and timedelta in hours
        new_dt = dates+hrs

        df['yyyymmdd'] = new_dt.dt.strftime('%Y%m%d')
        df['HHMMSS'] = new_dt.dt.strftime('%H%M%S')

        logging.info("Exiting get_datetimes()")

        return df

    def _create_status_flag(self, df):
        """
        Create Status Flag based on the values from Prep_QC_Mark,
        Prep_Use_Flag, and Analysis_Use_Flag.
        Args:
            df: (df) pandas dataframe populated with data from GSI
                diagnostic files
        Returns:
            df: (df) the same dataframe read in with a new column: 'StatusFlag'
        """
        logging.info("Working in create_status_flag()")

        # Create 'StatusFlag' column and fill with nans
        df['StatusFlag'] = np.nan

        # Obs used by GSI, Status_Flag=0
        df.loc[(df['Prep_QC_Mark'] <= 8) & (df['Analysis_Use_Flag'] == 1), 'StatusFlag'] = 0

        # Obs rejected by GSI, Status_Flag=0
        df.loc[(df['Prep_QC_Mark'] <= 8) & (df['Analysis_Use_Flag'] == -1), 'StatusFlag'] = 2

        # Obs never used by GSI, Status_Flag=3
        df.loc[(df['Prep_QC_Mark'] > 8) & (df['Prep_Use_Flag'] >= 100), 'StatusFlag'] = 3

        # Obs is flagged for non-use by the analysis, Status_Flag=3
        df.loc[df['Prep_QC_Mark'] >= 15, 'StatusFlag'] = 3

        # Obs rejected by SDM or CQCPROF, Status_Flag=7
        df.loc[(df['Prep_QC_Mark'] >= 12) & (df['Prep_QC_Mark'] <= 14), 'StatusFlag'] = 7

        # Fill those that do not fit a condition with -999
        df.loc[df['StatusFlag'].isnull(), 'StatusFlag'] = -999

        logging.debug("Status Flag Counts:")
        logging.debug(f"{df['StatusFlag'].value_counts()}")
        logging.info("Exiting create_status_flag()")

        return df

    def _round_column(self, df, col):
        """
        Round column numbers to 4 decimal places.
        Input:
            df: dataframe with information
            col: column name to round data
        Output:
            df: dataframe with changed values in provided column
        """
        logging.debug("Working in round_column()")

        df[col] = df[col].map('{:,.4f}'.format)

        logging.debug("Exiting round_column()")

        return df

    def _create_conv_df(self, df):
        """
        Create dataframe for conventional data.
        """
        logging.info("Working in create_conv_df")

        # Add center_id
        df['Centre_id'] = 'NCEP'
        df['CodeType'] = 999

        # Remove unnecessary columns
        df.drop(['Observation_Type', 'Pressure', 'Time', 'Prep_QC_Mark',
                'Prep_Use_Flag', 'Analysis_Use_Flag', 'Datetime'],
                axis=1, inplace=True)

        # Rename columns
        df = df.rename({'Obs_Minus_Forecast_adjusted': 'Bg_dep',
                        'Latitude': 'latitude',
                        'Station_ID': 'Station_id'}, axis=1)

        # ordered columns
        cols = ['Station_id', 'yyyymmdd', 'HHMMSS', 'latitude', 'Longitude',
                'StatusFlag', 'Centre_id', 'var_id', 'Bg_dep', 'CodeType']

        df = df[cols]
        df = df.reset_index(drop=True)

        # Round given columns to four decimal places
        for col in ['latitude', 'Longitude', 'Bg_dep']:
            df = self._round_column(df, col)

        logging.info("Exiting create_conv_df()")

        return df

    def _create_sondes_df(self, df):
        """
        Create dataframe for sondes.
        """
        logging.info("Working in create_sondes_df()")

        stn_ids = df['Station_ID'].unique()

        df_list = []

        # Loop through stations and create individual dataframes
        # that grabs average stats from surface, troposphere, and
        # stratosphere
        for stn in stn_ids:
            logging.debug(f"Station ID: {stn}")

            d = {
                'var_id': [],
                'Mean_Bg_dep': [],
                'Std_Bg_dep': [],
                'Levels': [],
                'LastRepLevel': [],
                'StatusFlag': []
            }

            surf_lat = None
            surf_lon = None

            # Temporary dataframe of specific station data
            tmp = df.loc[df['Station_ID'] == stn]

            # Add pressure info if available
            if 110 in tmp['var_id'].unique():
                logging.debug(f"Variable: p")

                mean_bg_dep = tmp['Obs_Minus_Forecast_adjusted'].loc[tmp['var_id'] == 110].values[0]
                std_bg_dep = 0
                level = 'Surf'
                last_rep_lvl = -999.99
                status_flag = tmp['StatusFlag'].loc[tmp['var_id'] == 110].values[0]

                d['var_id'].append(110)
                d['Mean_Bg_dep'].append(mean_bg_dep)
                d['Std_Bg_dep'].append(std_bg_dep)  # cannot compute std w/ one value so set to 0
                d['Levels'].append(level)
                d['LastRepLevel'].append(last_rep_lvl)
                d['StatusFlag'].append(status_flag)

                # surface lat and lon if exists
                surf_lat = tmp['Latitude'].loc[tmp['var_id'] == 110].values[0]
                surf_lon = tmp['Longitude'].loc[tmp['var_id'] == 110].values[0]

                logging.debug("Mean_Bg_dep, Std_Bg_dep, Levels, LastRepLevel, StatusFlag")
                logging.debug(f"{mean_bg_dep}, {std_bg_dep}, {level}, {last_rep_lvl}, {status_flag}")

            # Get unique variable ID's and remove 110 (surface pressure)
            var_ids = sorted(tmp['var_id'].unique())
            var_ids.remove(110) if 110 in var_ids else var_ids

            for var in var_ids:
                logging.debug(f"Variable: {var}")

                # Surface
                
                # Find max pressure of the surface 110 value
                surf_p_max = tmp['Pressure'].loc[tmp['var_id'] == 110].max()

                if (110 in tmp['var_id'].unique() and
                    var in tmp['var_id'].loc[tmp['Pressure'] == surf_p_max].unique()):

                    surf_tmp = tmp.loc[(tmp['Pressure'] == surf_p_max) &
                                       (tmp['var_id'] == var)]

                    surf_omf = surf_tmp['Obs_Minus_Forecast_adjusted'].values.mean()
                    surf_std = 0  # cannot compute std w/ one value so set to 0
                    level = 'Surf'
                    last_rep_lvl = -999.99

                    # If at least one ob is used, we report the lowest Status Flag.
                    # Although it does not represent the whole column, it is what is
                    # required by the WDQMS team.
                    status_flag = surf_tmp['StatusFlag'].min()

                    d['var_id'].append(var)
                    d['Mean_Bg_dep'].append(surf_omf)
                    d['Std_Bg_dep'].append(surf_std)
                    d['Levels'].append(level)
                    d['LastRepLevel'].append(last_rep_lvl)
                    d['StatusFlag'].append(status_flag)

                    logging.debug("Mean_Bg_dep, Std_Bg_dep, Levels, LastRepLevel, StatusFlag")
                    logging.debug(f"{surf_omf}, {surf_std}, {level}, {last_rep_lvl}, {status_flag}")

                # Troposphere
                trop_tmp = tmp.loc[(tmp['var_id'] == var) &
                                   (tmp['Pressure'] >= 100)]

                if len(trop_tmp) > 0:
                    trop_avg_omf = trop_tmp['Obs_Minus_Forecast_adjusted'].mean()
                    trop_std_omf = trop_tmp['Obs_Minus_Forecast_adjusted'].std()
                    level = 'Trop'
                    # Get lowest p for entire atmosphere
                    last_rep_lvl = tmp['Pressure'].min()

                    # If at least one ob is used, we report the lowest Status Flag.
                    # Although it does not represent the whole column, it is what is
                    # required by the WDQMS team.
                    status_flag = trop_tmp['StatusFlag'].min()

                    d['var_id'].append(var)
                    d['Mean_Bg_dep'].append(trop_avg_omf)
                    d['Std_Bg_dep'].append(trop_std_omf)
                    d['Levels'].append(level)
                    d['LastRepLevel'].append(last_rep_lvl)
                    d['StatusFlag'].append(status_flag)

                    logging.debug("Mean_Bg_dep, Std_Bg_dep, Levels, LastRepLevel, StatusFlag")
                    logging.debug(f"{trop_avg_omf}, {trop_std_omf}, {level}, {last_rep_lvl}, {status_flag}")

                # Stratosphere
                stra_tmp = tmp.loc[(tmp['var_id'] == var) &
                                   (tmp['Pressure'] < 100)]

                if len(stra_tmp) > 0:
                    stra_avg_omf = stra_tmp['Obs_Minus_Forecast_adjusted'].mean()
                    stra_std_omf = 0 if len(stra_tmp == 1) else stra_tmp['Obs_Minus_Forecast_adjusted'].std()
                    level = 'Stra'
                    # Get lowest p for entire atmosphere
                    last_rep_lvl = tmp['Pressure'].min()

                    # If at least one ob is used, we report the lowest Status Flag.
                    # Although it does not represent the whole column, it is what is
                    # required by the WDQMS team.
                    status_flag = stra_tmp['StatusFlag'].min()

                    d['var_id'].append(var)
                    d['Mean_Bg_dep'].append(stra_avg_omf)
                    d['Std_Bg_dep'].append(stra_std_omf)
                    d['Levels'].append(level)
                    d['LastRepLevel'].append(last_rep_lvl)
                    d['StatusFlag'].append(status_flag)

                    logging.debug("Mean_Bg_dep, Std_Bg_dep, Levels, LastRepLevel, StatusFlag")
                    logging.debug(f"{stra_avg_omf}, {stra_std_omf}, {level}, {last_rep_lvl}, {status_flag}")

            sub_df = pd.DataFrame.from_dict(d)
            sub_df['Station_id'] = stn
            # Add lats and lons
            lat = surf_lat if surf_lat else tmp['Latitude'].value_counts().index[0]
            lon = surf_lon if surf_lon else tmp['Longitude'].value_counts().index[0]
            sub_df['latitude'] = lat
            sub_df['Longitude'] = lon
            # add datetime
            str_datetime = str(tmp['Datetime'].values[0])
            sub_df['yyyymmdd'] = str_datetime[:-2]
            sub_df['HHMMSS'] = str_datetime[-2:] + '0000'

            df_list.append(sub_df)

        df = pd.concat(df_list)
        df['Centre_id'] = 'NCEP'
        df['CodeType'] = 999
        df = df.dropna()

        # Ordered columns
        cols = ['Station_id', 'yyyymmdd', 'HHMMSS', 'latitude', 'Longitude',
                'StatusFlag', 'Centre_id', 'var_id', 'Mean_Bg_dep', 'Std_Bg_dep',
                'Levels', 'LastRepLevel', 'CodeType']

        df = df[cols]
        df = df.reset_index(drop=True)

        # Round given columns to four decimal places
        for col in ['latitude', 'Longitude', 'Mean_Bg_dep', 'Std_Bg_dep', 'LastRepLevel']:
            df = self._round_column(df, col)

        logging.info("Exiting create_sondes_df()")

        return df

    def _genqsat(self, df):
        """
        Calculates new background departure values for specific humidity (q)
        by calculating saturation specific humidity from corresponding temperature
        and pressure values.

        bg_dep = (q_obs/qsat_obs)-(q_ges/qsat_ges)

        q_obs : measured q obs
        qsat_obs : calculated saturation q
        q_ges : q_obs minus q background error from GSI diagnostic file
        qsat_ges : calculated saturation q using temperature obs minus
                   temperature background error from GSI diagnostic file

        Args:
            df : (df) pandas dataframe populated with data from GSI
                 diagnostic files
            wdqms_type : (str) wdqms type file being created
        Returns:
            df: (df) the same dataframe read in with new background
                departure values for humidity data
        """
        logging.info("Working in genqstat()")

        # Get variable type specific to WDQMS type
        q = self.wdqms_type_dict[self.wdqms_type]['variable_ids']['q']
        t = self.wdqms_type_dict[self.wdqms_type]['variable_ids']['t']

        # Create two dataframes, one for q vales and one for t values
        q_df = df.loc[(df['var_id'] == q)]
        t_df = df.loc[(df['var_id'] == t)]

        # Find where stations are the same
        stn_ids = np.intersect1d(t_df.Station_ID, q_df.Station_ID)

        # loop through stations, calculate saturation specific humidity,
        # and replace background departure values from NetCDF file with
        # new ones calculated using the temperature obs and best temperature guess
        for stn in stn_ids:
            logging.debug(f"Station ID: {stn}")

            t_tmp = t_df.loc[(t_df['Station_ID'] == stn)]
            q_tmp = q_df.loc[(q_df['Station_ID'] == stn)]

            t_tmp = t_tmp.loc[(np.in1d(t_tmp['Time'], q_tmp['Time'])) &
                              (np.in1d(t_tmp['Pressure'], q_tmp['Pressure'])) &
                              (np.in1d(t_tmp['Latitude'], q_tmp['Latitude'])) &
                              (np.in1d(t_tmp['Longitude'], q_tmp['Longitude']))]

            q_obs = q_tmp['Observation'].to_numpy() * 1.0e6
            q_ges = (q_tmp['Observation'].to_numpy() -
                     q_tmp['Obs_Minus_Forecast_adjusted'].to_numpy()) * 1.0e6
            t_obs = t_tmp['Observation'].to_numpy() - 273.16
            t_ges = (t_tmp['Observation'].to_numpy() -
                     t_tmp['Obs_Minus_Forecast_adjusted'].to_numpy()) -273.16
            pressure = q_tmp['Pressure'].to_numpy()

            qsat_obs = self._temp_2_saturation_specific_humidity(pressure, t_obs)
            qsat_ges = self._temp_2_saturation_specific_humidity(pressure, t_ges)

            bg_dep = (q_obs/qsat_obs)-(q_ges/qsat_ges)

            logging.debug(f"Original q background departure: {q_tmp['Obs_Minus_Forecast_adjusted'].values}")
            logging.debug(f"New q background departure: {bg_dep}")

            # Replace the current background departure with the new calculated one
            df.loc[(df['var_id'] == self.wdqms_type_dict[self.wdqms_type]['variable_ids']['q']) &
                   (df['Station_ID'] == stn),
                   'Obs_Minus_Forecast_adjusted'] = bg_dep

        logging.info("Exiting genqstat()")

        return df

    def _temp_2_saturation_specific_humidity(self, pres, tsen):
        """
        Uses pressure and temperature arrays to calculate saturation
        specific humidity.
        Args:
            pres: (array) array of pressure obs
            tsen: (array) array of temperature obs in Celsius
        Returns:
            qsat_array: (array) corresponding calculated sat. spec. humidity
        """
        logging.debug("Working in temp_2_saturation_specific_humidity()")

        ttp   = 2.7316e2      # temperature at h2o triple point (K)
        psat  = 6.1078e1      # pressure at h2o triple point  (Pa)
        cvap  = 1.8460e3      # specific heat of h2o vapor (J/kg/K)
        csol  = 2.1060e3      # specific heat of solid h2o (ice)(J/kg/K)
        hvap  = 2.5000e6      # latent heat of h2o condensation (J/kg)
        hfus  = 3.3358e5      # latent heat of h2o fusion (J/kg)
        rd    = 2.8705e2
        rv    = 4.6150e2
        cv    = 7.1760e2
        cliq  = 4.1855e3

        dldt  = cvap-cliq
        dldti = cvap-csol
        hsub  = hvap+hfus
        tmix  = ttp-20.
        xa    = -(dldt/rv)
        xai   = -(dldti/rv)
        xb    = xa+hvap/(rv*ttp)
        xbi   = xai+hsub/(rv*ttp)
        eps   = rd/rv
        omeps = 1.0-eps

        tdry = tsen+ttp
        tdry = np.array([1.0e-8 if np.abs(t) < 1.0e-8 else t for t in tdry])

        tr = ttp/tdry

        qsat_array = []

        # Loop through temperatures and appropriate indexes to solve qsat
        for idx, t in enumerate(tdry):
            # Get correct estmax and es values based on conditions
            if t >= ttp:
                estmax = psat * (tr[idx]**xa) * np.exp(xb*(1.0-tr[idx]))
                es = estmax
            elif t < tmix:
                estmax = psat * (tr[idx]**xa) * np.exp(xbi*(1.0-tr[idx]))
                es = estmax
            else:
                w = (t-tmix) / (ttp-tmix)
                estmax = w * psat * (tr[idx]**xa) * np.exp(xb*(1.0-tr[idx])) \
                         + (1.0-w) * psat * (tr[idx]**xai) * np.exp(xbi*(1.0-tr[idx]))

                es = w * psat * (tr[idx]**xa) * np.exp(xb*(1.0-tr[idx])) \
                     + (1.0-w) * psat * (tr[idx]**xai) * np.exp(xbi*(1.0-tr[idx]))

            pw = pres[idx]
            esmax = pw

            esmax = np.min([esmax, estmax])
            es2 = np.min([es, esmax])

            qsat = eps * es2 / ((pw*10.0) - (omeps*es2))
            qsat2 = qsat*1e6

            qsat_array.append(qsat2)

        logging.debug("Exiting temp_2_saturation_specific_humidity()")

        return np.array(qsat_array)

    def _read_gsi_diag(self, file):
        """
        Reads the data from the conventional diagnostic file during
        initialization into a pandas dataframe.
        Args:
            file : (str) netCDF GSI diagnostic file
        Returns:
            df : (dataframe) pandas dataframe populated with data from
                 netCDF GSI diagnostic file
        """
        logging.debug("Working in read_gsi_diag()")

        filename = os.path.splitext(Path(file).stem)[0]
        logging.debug(f'Filename: {filename}')

        variable = filename.split('_')[2]
        logging.debug(f'Variable: {variable}')

        df_dict = {}

        column_list = ['Station_ID', 'Observation_Class', 'Observation_Type',
                       'Latitude', 'Longitude', 'Pressure', 'Time', 'Prep_QC_Mark', 'Prep_Use_Flag',
                       'Analysis_Use_Flag', 'Observation', 'Obs_Minus_Forecast_adjusted']

        # Grab datetime from file
        datetime = self._grab_netcdf_data(file, 'Datetime')

        if variable == 'uv':
            for wtype in ['u', 'v']:
                df_dict[wtype] = {}
                for col in column_list:
                    col = f'{wtype}_'+col if col == 'Observation' else col
                    col = f'{wtype}_'+col if col == 'Obs_Minus_Forecast_adjusted' else col
                    data = self._grab_netcdf_data(file, col)
                    df_dict[wtype][col] = data

            # Need to separate the u and v dataframes to concatenate them
            u_df = pd.DataFrame(df_dict['u'])
            u_df = u_df.rename({'Observation_Class': 'var_id',
                                'u_Observation': 'Observation',
                                'u_Obs_Minus_Forecast_adjusted': 'Obs_Minus_Forecast_adjusted'},
                               axis=1)
            u_df['var_id'] = self.wdqms_type_dict[self.wdqms_type]['variable_ids']['u']

            v_df = pd.DataFrame(df_dict['v'])
            v_df = v_df.rename({'Observation_Class': 'var_id',
                                'v_Observation': 'Observation',
                                'v_Obs_Minus_Forecast_adjusted': 'Obs_Minus_Forecast_adjusted'},
                               axis=1)
            v_df['var_id'] = self.wdqms_type_dict[self.wdqms_type]['variable_ids']['v']

            df = pd.concat([u_df, v_df])

        else:
            for col in column_list:
                data = self._grab_netcdf_data(file, col)
                df_dict[col] = data

            df = pd.DataFrame(df_dict)
            df = df.rename({'Observation_Class': 'var_id'}, axis=1)
            df['var_id'] = self.wdqms_type_dict[self.wdqms_type]['variable_ids'][variable]

        # Add datetime column to dataframe
        df['Datetime'] = datetime

        # Subtract longitudes > 180 by 360 to be negative
        df.loc[df['Longitude'] > 180, 'Longitude'] -= 360

        logging.debug("Exiting read_gsi_diag()")

        return df

    def _grab_netcdf_data(self, file, var):
        """
        Opens and grabs data based on column name.
        Args:
            file : (str) netCDF GSI file
            var  : (str) the variable to be extracted
        Returns:
            data : (array) values from the specified variable
        """
        logging.debug('Working in grab_netcdf_data()')

        with Dataset(file, mode='r') as f:
            # Station_ID and Observation_Class variables need
            # to be converted from byte string to string
            if var == 'Datetime':
                data = f.date_time

            elif var in ['Station_ID', 'Observation_Class']:
                data = f.variables[var][:]
                data = [i.tobytes(fill_value='        ', order='C')
                        for i in data]
                data = np.array(
                    [''.join(i.decode('UTF-8', 'ignore').split())
                     for i in data])

            # Grab variables with only 'nobs' dimension
            elif len(f.variables[var].shape) == 1:
                data = f.variables[var][:]

        logging.debug("Exiting grab_netcdf_data()")

        return data

    def _df_to_csv(self, df):
        """
        Produce output .csv file from dataframe.
        """
        logging.info("Working in df_to_csv()")
        logging.info(f'Coverting dataframe to .csv file for {self.wdqms_type} data ...')

        # Write dataframe to .csv
        date = self.datetime[:-2]
        cycle = self.datetime[-2:]

        hr_range = {
            '00': ['21', '03'],
            '06': ['03', '09'],
            '12': ['09', '15'],
            '18': ['15', '21']
        }

        filename = f'{self.outdir}/NCEP_{self.wdqms_type}_{date}_{cycle}.csv'

        f = open(filename, 'a')
        f.write(f"# TYPE={self.wdqms_type}\n")
        f.write(f"#An_Date= {date}\n")
        f.write(f"#An_time= {cycle}\n")
        f.write(f"#An_range=[ {hr_range[cycle][0]} to {hr_range[cycle][-1]} )\n")
        f.write("#StatusFlag: 0(Used);1(Not Used);2(Rejected by DA);"
                "3(Never Used by DA);4(Data Thinned);5(Rejected before DA);"
                "6(Alternative Used);7(Quality Issue);8(Other Reason);9(No content)\n")
        df.to_csv(f, index=False)
        f.close()

        logging.info(f'{filename} file created.')
        logging.info('Exiting df_to_csv()')

        return filename
