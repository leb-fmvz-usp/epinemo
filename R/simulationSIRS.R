#' @title Simulating the SIRS model in a network
#' 
#' @description A function to run the simulation of disease spread in a network
#' using the SIRS (Susceptible-Infected-Recovered-Susceptible) model.
#' 
#' @param A An adjacency \code{\link{matrix}}
#' @param pspread Probability of disease spread from an infected to a susceptible node
#' @param tSim Simulation time
#' @param I Vector of infected nodes (initial condition)
#' @param tImin Minimum time a node remains infected
#' @param tImax Maximum time a node remains infected
#' @param R Vector of recovered nodes (initial condition) 
#' @param tRmin Minimum time a node remains immune
#' @param tRmax Maximum time a node remains immune
#' @param Control Vector of nodes under a control strategy
#' 
#' @details This function runs the simulation of disease spread in a network
#' using the SIRS model. For each time step, the vectors of infected, susceptible
#' and recovered nodes are updated, considering that 
#' there is a probability \code{pspread} of disease spread. 
#' Infected nodes remain infected during a time randomly sampled 
#' between \code{tImin} and \code{tImax}. Recovered nodes remain immune 
#' during a time randomly sampled between \code{tRmin}
#' and \code{tRmax}. Nodes under a control strategy (\code{Control}) 
#' are not susceptible to infection.
#' 
#' @return  A list of 
#' \item{M_Sim_I}{a matrix of infected nodes for each time step.} 
#' \item{M_Sim_R}{a matrix of recovered nodes for each time step.}
#' 
#' @importFrom stats runif
#' 
#' @references 
#' [1] Ossada R, Grisi-Filho JHH, Ferreira F, Amaku M (2013). 
#' "Modeling the Dynamics of Infectious
#' Diseases in Different Scale-Free Networks with the Same Degree Distribution." 
#' Advanced Studies in Theoretical Physics, 7, 759-771. 
#' \doi{10.12988/astp.2013.3674}
#' 
#' [2] Ossada R (2015). "Modelagem de Medidas de Controle em Redes de Movimentacao de Animais."
#' PhD Thesis. Sao Paulo, School of Veterinary Medicine, University of Sao Paulo. 
#' \doi{10.11606/T.10.2015.tde-06112015-111048}
#' 
#' @export
#' @examples 
#' # Generate an arbitrary 200 by 200 adjacency matrix with zeros and ones
#' # Remove loops
#' A <- matrix(rbinom(200 * 200, 1, 0.1), ncol = 200, nrow = 200)
#' diag(A) <- 0
#' 
#' # Setting the parameters
#' pspread <- 0.1
#' tImin <- 5
#' tImax <- 8
#' tRmin <- 1
#' tRmax <- 10
#' tSim <- 100
#' 
#' # Setting the initial conditions for infected, recovered and controlled nodes
#' num_infected <- 2 # initial number of infected nodes
#' I <- rep(x = 0, times = nrow(A))
#' I[1:num_infected] <- 1
#' I <- sample(I)
#' R <- rep(x = 0, times = nrow(A))
#' Control <- rep(x = 0, times = nrow(A))
#' 
#' # Run the simulation
#' sim <- simulationSIRS(A = A, pspread = pspread, tSim = tSim,
#'                       I = I, tImin = tImin, tImax = tImax, 
#'                       R = R, tRmin = tRmin, tRmax = tRmax, Control = Control)
#'
#' # Plot the prevalence over time
#' plot(colMeans(sim[[1]]>0), xlab = "Time", ylab = "Prevalence")
#'                                                    

simulationSIRS <- function(A, pspread, tSim, I, tImin, tImax, R, tRmin, tRmax, Control)
{
    # Builds the simulation matrix where each row represents a vertex and each column represents a time step of the simulation...
    # ... for the 'infected' vertices
    M_Sim_I = matrix(data=NA, nrow=nrow(A), ncol=(tSim+1) );

    # ... for the 'recovered' vertices
    M_Sim_R = matrix(data=NA, nrow=nrow(A), ncol=(tSim+1) );

    # Assigning the vectors I and R to tI and tR
    tI <- I
    tR <- R
    
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
        Pvector = I %*% A;

        # Vector with the probability of each vertex getting infected
        Pvertex = 1 - (1-pspread)^Pvector;
                
        # Generating a vector with random numbers based on a uniform distribution [0,1]
        Prand = runif(n=length(A[1,]), min=0, max=1);

        # If Prand(i) <= Pvertex(i), the vertex becames 'infected'
        Pinfection = as.numeric(Prand<=Pvertex);
        
        # Vertices under a control strategy may not be infected
        Pinfection = Pinfection*!(Control);

        # Creating the new 'infected' vector: S=(0,1) AND Pinfection=(0,1)
        Inew = as.numeric(S & (Pinfection) );

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
        # Nodes under a control strategy are also included
        tR = tRnew + aux_tR + Control;

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




