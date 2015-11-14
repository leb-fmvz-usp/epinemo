#' @title Function to run the simulation of disease spread considering two networks
#' 
#' @description Function to run the simulation of disease spread considering two networks
#' 
#' @param Two adjacency \code{\link{matrix}}
#' 
#' @details Function to run the simulation of disease spread on a network
#' 
#' @return \code{\link{list}}. The first element, a
#'         \code{\link{matrix}} of Infected, and the second element, a
#'         \code{\link{matrix}} of Recovered.
#' 
#' @references 
#' 
#' 
#' \url{http://github.com/leb-fmvz-usp.github/epinemo}
#' @export
#' @examples 
#' # Loading data from....
#' 
#' # call function
#' 
#'                                                    

# SIRS2nets.R
# Original version: Raul Ossada
# Version: 03/Nov/2012
# Changes: 
# Marcos Amaku 11/Nov/2015 - including control

# Function to run the simulation of disease spread considering two networks

#####################################################################################

SIRS2nets <- function(M_adj1, M_adj2, pspread1, pspread2, tSim, tI, tImin, tImax,  tR, tRmin, tRmax, tcontrol)
{
    # Builds the simulation matrix where each row represents a vertex and each column represents a time step of the simulation...
    # ... for the 'infected' vertices
    M_Sim_I = matrix(data=NA, nrow=nrow(M_adj1), ncol=(tSim+1) );

    # ... for the 'recovered' vertices
    M_Sim_R = matrix(data=NA, nrow=nrow(M_adj1), ncol=(tSim+1) );

    # For each time step...
    for( i in 1:tSim )
    {
        # ... assembles the tI vector to the corresponding column of the simulation matrix
        M_Sim_I[,i] = tI;

        # ... assembles the tR vector to the corresponding column of the simulation matrix
	  M_Sim_R[,i] = tR;

        # Identify the vertices that are at the 'infected', 'recovered' and 'susceptible' state
        I = as.numeric(tI>0);
        R = as.numeric(tR>0);
        S = as.numeric(!I&!R);

        # Multiply the 'infected' vector by the adjacency matrix, generating the potential infection vector
        Pvector1 = I %*% M_adj1;
        Pvector2 = I %*% M_adj2;

        # Vector with the probability of each vertex getting infected
        Pvertex1 = 1 - (1-pspread1)^Pvector1;
        Pvertex2 = 1 - (1-pspread2)^Pvector2;
                
        # Generating a vector with random numbers based on a uniform distribution [0,1]
        Prand1 = runif(n=length(M_adj1[1,]), min=0, max=1);
        Prand2 = runif(n=length(M_adj2[1,]), min=0, max=1);

        # If Prand(i) <= Pvertex(i), the vertex becames 'infected'
        Pinfection1 = as.numeric(Prand1<=Pvertex1);
        Pinfection2 = as.numeric(Prand2<=Pvertex2);
        
        # Vertices under a control strategy may not be infected
        Pinfection1 = Pinfection1*!(tcontrol);
        Pinfection2 = Pinfection2*!(tcontrol);

        # Creating the new 'infected' vector: S=(0,1) AND Pinfection=(0,1)
        Inew = as.numeric(S & (Pinfection1|Pinfection2) );

	  # If a vertex is about to be 'recovered', mark it as a new 'recovered' vertex
        Rnew = rep( 0, length(R) );
        Rnew[tI==1] = 1;

        # Update the 'tI vector' decreasing 1 for each element in it, denoting the passage of 1 time step
        temp_tI = tI-1;
        # Check if any vertex got a negative time duration and if that happened turn it back to zero
        tInew = as.numeric(temp_tI<0) + temp_tI;

        # Update the 'tR vector' decreasing 1 for each element in it, denoting the passage of 1 time step
        temp_tR = tR-1;
        # Check if any vertex got a negative time duration and if that happened turn it back to zero
        tRnew = as.numeric(temp_tR<0) + temp_tR;


        # Give a random duration to the new 'infected' vertices
        aux_tI = rep( 0, length(Inew) );
        
        # Check if the time of 'infection' is fixed or variable
        if( tImin != tImax )
        {
            aux_tI[ Inew>0 ] = sample(x=(tImin:tImax), size=length( which(Inew!=0) ), replace=TRUE);
        }else
        {
            aux_tI[ Inew>0 ] = rep(x=tImax, times=length( which(Inew!=0) ) );
        }


        # Give a random duration to the new 'recovered' vertices 
        aux_tR = rep( 0, length(Rnew) );
        
        # Check if the time of 'recovery' is fixed or variable
        if( tRmin != tRmax )
        {
            aux_tR[ Rnew>0 ] = sample(x=(tRmin:tRmax), size=length( which(Rnew!=0) ), replace=TRUE);
        }else
        {
            aux_tR[ Rnew>0 ] = rep(x=tRmax, times=length( which(Rnew!=0) ) );
        }    


        # Merge the new 'infected' vertices with the older ones
        tI = tInew + aux_tI;

        # Merge the new 'recovered' vertices with the older ones
        # Vertices under a control strategy are also included
        tR = tRnew + aux_tR + tcontrol;

    }
    # Inputs the results of the last time step of the simulation, in the simulation matrix
    # ... for the 'infected' vertices
    M_Sim_I[,i+1] = tI;

    # Inputs the results of the last time step of the simulation, in the simulation matrix
    # ... for the 'recovered' vertices
    M_Sim_R[,i+1] = tR;

    # Assigns the simulation matrix to the variable to be returned, case no options were selected
    answer = list(M_Sim_I, M_Sim_R);
    
    # Return the answer asked by the user
    return( answer );
}



