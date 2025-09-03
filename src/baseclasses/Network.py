#!/usr/bin/env python3
import logging
import pandas as pd
import sys
from typing import Type
from . import Group
from .Group import RiskGroup, VaccineGroup, Compartments
from .Node import Node
from .PopulationCompartments import PopulationCompartments
from .TravelFlow import TravelFlow

logger = logging.getLogger(__name__)


class Network:

    def __init__(self, compartment_labels:list):
        self.nodes = []
        self.travel_flow_data = None
        self.total_population = 0
        self.df_county_age_matrix = []

        # Set the active Compartments enum globally (proxied)
        self.compartment_labels = compartment_labels
        Group.set_compartments(self.compartment_labels)

        # Get Compartments enumeration for simulator to check totals
        self.comp_index = {c.name: c.value for c in Compartments}

        self.num_disease_compartments = len(self.compartment_labels)
        logger.info(f'instantiated Network object with {self.get_number_of_nodes()} nodes')
        return


    def __str__(self) -> str:
        return(f'Network:Nodes={self.get_number_of_nodes()}')


    def load_population_file(self, filename:str):
        """
        Loads population file into dataframe to prepare for loading into Nodes

        The population file should contain a header row followed by a row for
        each node (e.g. county, city, zip code, etc.). The first column should
        be the fips ID, and the rest of the columns should be age groups.
        """
        try:
            self.df_county_age_matrix = pd.read_csv(filename)
        except FileNotFoundError as e:
            raise Exception(f'Could not open {filename}') from e
            sys.exit(1)
        logger.info(f'loaded population data from {filename} into Network')
        logger.debug(f'{self.df_county_age_matrix}')
        return

            
    def population_to_nodes(self, high_risk_ratios:list):
        """
        Store population data as list of nodes. Needs high_risk_ratios to
        partition between high risk and low risk compartments
        """
        for index, row in self.df_county_age_matrix.iterrows():
            this_index = index
            this_id = row.iloc[0]
            # TODO FIPS is only for census boundaries, ZIP Code/ZCTA is not a FIPS code
            # Would be better to remove FIPS altogether and make passed id a FIPS code
            if len(this_id) <= 3:
                # 3-digit county code, add Texas state prefix
                this_fips = 48000 + int(this_id)
            else:
                # Any other length ID we'll keep and call it a fips
                this_fips = int(this_id)
            this_group = list(row[1:])
            this_compartment = PopulationCompartments(this_group, high_risk_ratios)
            this_node = Node(this_index, this_id, this_fips, this_compartment)
            self._add_node(this_node)

        logger.info(f'converted population data into {self.get_number_of_nodes()} nodes')
        return


    def _add_node(self, node:Type[Node]):
        """
        Add one node object to the end of the list of nodes[]
        """
        self.nodes.append(node)
        return


    def add_travel_flow_data(self, travel_flow_data:Type[TravelFlow]):
        """
        Copy travel flow data onto Network object
        """
        self.travel_flow_data = travel_flow_data
        logger.info(f'added travel flow data to Network object')
        logger.debug(f'{self.travel_flow_data.shape}')
        logger.debug(f'{self.travel_flow_data}')
        return


    def get_number_of_nodes(self) -> int:
        """
        Return number of Nodes
        """
        return(len(self.nodes))


    def get_total_population(self) -> int:
        """
        Return total population across all Nodes in Network
        """
        total_population = 0
        for item in self.nodes:
            total_population += item.compartments.total_population
            logger.debug(f'population in this node is {item.compartments.total_population}')
        self.total_population = total_population
        return self.total_population


    def get_number_of_age_groups(self) -> int:
        """
        Return number of age groups
        """
        return self.nodes[0].compartments.number_of_age_groups


    def get_number_of_stratifications(self) -> int:
        """
        Return total number of stratifications in each Node in the Network
        num = age groups * risk groups * vaccine groups
        """
        return (self.nodes[0].compartments.number_of_age_groups * len(RiskGroup) * len(VaccineGroup))


    def get_node_index_by_id(self, node_id:int) -> int:
        """
        Given a location ID, return the index of the Node in the list of nodes
        """
        for index, node in enumerate(self.nodes):
            if node.node_id == node_id:
                logging.debug(f'found node {node_id} at index {index}')
                return index
        raise Exception(f'could not find node {node_id}')


