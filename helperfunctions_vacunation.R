##########################################################################
# Funciones auxiliares para calcular transicion de estados, por vacunacion
##########################################################################

set.seed(1)

# Funcion que calcula si la persona tiene problemas metabolicos
problemas_metabolicos <- function()
{
    # Del 13 al 56 porciento tienen problemas metabolicos
    porcentaje_prob <- runif(1, .13, .56)
    # De ese porcentaje "x" calculamos la probabilidad de que tengan problemas metabolicos
    con_problemas_metabolicos <- sample(c(0,1), size=1, replace=TRUE, prob=c(1-porcentaje_prob,porcentaje_prob))
    return(con_problemas_metabolicos)
}


# Funcion que calcula si una persona se vacuna y es efectiva o no
calcular_vacunacion_susceptible <- function(edad)
{
    se_vacuna <- 0

    if((edad>=0.6 & edad<=5) | (edad>=60))
    {
        # se vacuna si o si, solo debemos verificar si se aplica bien (si hubo buen manejo)
        vacuna_bien_aplicada <- sample(c(0,1), size=1, replace=TRUE, prob=c(0.01,0.99))
        if (vacuna_bien_aplicada == 1)
        {
            se_vacuna <- 1
        }
    } else 
    {
        # solo se vacunan si tienen problemas metabolicos
        if (problemas_metabolicos() == 1)
        {
            # Si se vacuna, y ahora verificamos que la vacuna sea bien aplicada (si hubo buen manejo)
            vacuna_bien_aplicada <- sample(c(0,1), size=1, replace=TRUE, prob=c(0.01,0.99))
            if (vacuna_bien_aplicada == 1)
            {
                se_vacuna <- 1
            }
        }
    }
    return(se_vacuna)
}
