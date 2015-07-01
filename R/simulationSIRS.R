#' @title Makes simulations in a network
#' 
#' @description Function to simulate disease spread in a network
#' 
#' @param M_adjc Adjacency \code{\link{matrix}} 
#' 
#' @param pspreadc \code{\link{numeric}}, indicates the probability of spread
#' 
#' @param tSim \code{\link{numeric}}, indicates the simulation time
#' 
#' @param tI \code{\link{vector}}, indicates the infected nodes at t = 0
#' 
#' @param tImin \code{\link{numeric}}, indicates the minimum duration of infection
#' 
#' @param tImax \code{\link{numeric}}, indicates the maximum duration of infection
#' 
#' @param tR \code{\link{vector}}, indicates the recovered nodes at t = 0
#' 
#' @param tRmin \code{\link{numeric}}, indicates the minimum duration of recovery
#' 
#' @param tRmax \code{\link{numeric}}, indicates the maximum duration of recovery
#' 
#' @details SIRS model
#' 
#' @return \code{\link{list}} with two elements. The first, M_Sim_I, M_Sim_R
#'         \code{*$M_Sim_I}, with one column for each time step and one line for each node. Cells indicate the node's state
#'         (infected or not) in a given time step.
#'         \code{*$M_Sim_R}, with one column for each time step and one line for each node. Cells indicate the node's state
#'         (recovered or not) in a given time step.
#' 
#' @references 
#' 
#' \url{http://github.com/leb-fmvz-usp.github/epinemo}
#' @export
#' @examples 
#' 
#' simulationSIRS( matriz, pspreadc = probabilidade, tSim = tsim, tI = tI, tImin = timin, tImax = timax, tR = tR, tRmin = trmin, tRmax = trmax)
#' 


simulationSIRS <- function(M_adjc, pspreadc, tSim, tI, tImin, tImax,  tR, tRmin, tRmax)
{
    # Builds the simulation matrix where each row represents a vertex and each column represents a time step of the simulation...
    # ... for the 'infected' vertices
    M_Sim_I = matrix(data=NA, nrow=nrow(M_adjc), ncol=(tSim+1) );

    # ... for the 'recovered' vertices
    M_Sim_R = matrix(data=NA, nrow=nrow(M_adjc), ncol=(tSim+1) );

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
        Pvectorc = I %*% M_adjc;

        # Vector with the probability of each vertex getting infected
        Pvertexc = 1 - (1-pspreadc)^Pvectorc;
                
        # Generating a vector with random numbers based on a uniform distribution [0,1]
        Prandc = runif(n=length(M_adjc[1,]), min=0, max=1);

        # If Prand(i) <= Pvertex(i), the vertex becames 'infected'
        Pinfectionc = as.numeric(Prandc<=Pvertexc);

        # Creating the new 'infected' vector: S=(0,1) AND Pinfection=(0,1)
        Inew = as.numeric(S & (Pinfectionc) );

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
        tR = tRnew + aux_tR;

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




