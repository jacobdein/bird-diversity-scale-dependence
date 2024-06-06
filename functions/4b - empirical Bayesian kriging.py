"""
Predicts values over an area with observed values using Empirical Bayesian 
kriging as implemented in ArcGIS

"""

import argparse
import arcpy
from os import path

arcpy.env.overwriteOutput = True

def execute_ebk(args):

    # Compute ArcGIS EBK geostatistical layer
    arcpy.EmpiricalBayesianKriging_ga(
        in_features=args.obs_pts,
        z_field=args.z_field,
        out_ga_layer='EBK_Layer',
        transformation_type='LOGEMPIRICAL',
        max_local_points=100,
        overlap_factor=1,
        number_semivariograms=1000,
        search_neighborhood=arcpy.SearchNeighborhoodStandardCircular(radius=1000, sectorType='Full'),
        output_type='PREDICTION',
        semivariogram_model_type='K_BESSEL'
    )

    # Write in-memory layer to disk
    arcpy.SaveToLayerFile_management(in_layer='EBK_Layer', 
                                     out_layer=args.output_layer)
    
    # Extract EBK predictions to points
    arcpy.ga.GALayerToPoints(
        in_geostat_layer='EBK_Layer',
        in_locations=args.pre_pts,
        z_field=args.z_field if args.validate else None,
        out_feature_class=args.output_pts,
        append_all_fields=True
    )

if __name__ == "__main__":
    parser = argparse.ArgumentParser("Predicts values over an area with observed values using Empirical Bayesian kriging as implemented in ArcGIS")

    # Required positional argument with path to input features
    parser.add_argument("obs_pts", help="Path to input observations")

    # Required positional argument with name of data 'z' field
    parser.add_argument("z_field", help="Name of data field to krige")

    # Required positional argument with path to prediction points
    parser.add_argument("pre_pts", help="Path to points to predict value")

    # Required positional argument with path to output directory
    parser.add_argument("output_layer", help="Path to write EBK layer")

    # Required positional argument with path to output points with extracted values
    parser.add_argument("output_pts", help="Path to write points with extracted values")

    # Optional flag indicating whether to validate result based on observed values
    parser.add_argument('--validate', dest='validate', action='store_true', help='Validate result based on observed values')

    args = parser.parse_args()

    execute_ebk(args)
