library(jsonlite)

model = readRDS("model.rds")

#* Returns the probablity that a person will seek treatment for their health condition
#* @param Age Age
#* @param Gender Gender (M|F|O)
#* @param Region Country
#* @param state State
#* @param family_history Do you have a family history of mental illness?
#* @param work_interfere If you have a mental health condition, do you feel that it interferes with your work?
#* @param no_employees How many employees does your company or organization have?
#* @param remote_work Do you work remotely (outside of an office) at least 50% of the time?
#* @param tech_company Is your employer primarily a tech company/organization?
#* @param benefits Does your employer provide mental health benefits?
#* @param care_options Do you know the options for mental health care your employer provides?
#* @param wellness_program Has your employer ever discussed mental health as part of an employee wellness program?
#* @param seek_help Does your employer provide resources to learn more about mental health issues and how to seek help?
#* @param anonymity Is your anonymity protected if you choose to take advantage of mental health or substance abuse treatment resources?
#* @param leave How easy is it for you to take medical leave for a mental health condition?
#* @param mental_health_consequence Do you think that discussing a mental health issue with your employer would have negative consequences?
#* @param phys_health_consequence Do you think that discussing a physical health issue with your employer would have negative consequences?
#* @param coworkers Would you be willing to discuss a mental health issue with your coworkers?
#* @param supervisor Would you be willing to discuss a mental health issue with your direct supervisor(s)?
#* @param mental_health_interview Would you bring up a mental health issue with a potential employer in an interview?
#* @param phys_health_interview Would you bring up a physical health issue with a potential employer in an interview?
#* @param mental_vs_physical Do you feel that your employer takes mental health as seriously as physical health?
#* @param obs_consequence Have you heard of or observed negative consequences for coworkers with mental health conditions in your workplace?
#* @post /treatmentprobablity
function(
    Age,
    Gender,
    Region,
    state,
    family_history,
    work_interfere,
    no_employees,
    remote_work,
    tech_company,
    benefits,
    care_options,
    wellness_program,
    seek_help,
    anonymity,
    leave,
    mental_health_consequence,
    phys_health_consequence,
    coworkers,
    supervisor,
    mental_health_interview,
    phys_health_interview,
    mental_vs_physical,
    obs_consequence
) {
    newdata <- data.frame(
        Age = as.numeric(Age),
        Gender = factor(Gender, levels = c("F", "M", "O")),
        Region = factor(Region, levels = c("US", "Europe", "NorthAmerica_other", "Oceana", "Asia", "South_Central_America", "Africa")),
        state = factor(state, levels = c("NA", "AL", "AZ", "AR", "CA", "CO", "CT", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")),
        family_history = factor(family_history, levels = c("No", "Yes")),
        work_interfere = factor(work_interfere, levels = c("NA", "Never", "Rarely", "Sometimes", "Often")),
        no_employees = factor(no_employees, levels = c("NA", "1 to 5", "6 to 25", "26 to 100", "100 to 500", "500 to 1000", "More than 1000")),
        remote_work = factor(remote_work, levels = c("No", "Yes")),
        tech_company = factor(tech_company, levels = c("No", "Yes")),
        benefits = factor(benefits, levels = c("No", "Don't know", "Yes")),
        care_options = factor(care_options, levels = c("No", "Not Sure", "Yes")),
        wellness_program = factor(wellness_program, levels = c("No", "Don't know", "Yes")),
        seek_help = factor(seek_help, levels = c("No", "Don't know", "Yes")),
        anonymity = factor(anonymity, levels = c("No", "Don't know", "Yes")),
        leave = factor(leave, levels = c("Don't know", "Very difficult", "Somewhat difficult", "Somewhat easy", "Very easy")),
        mental_health_consequence = factor(mental_health_consequence, levels = c("Maybe", "No", "Yes")),
        phys_health_consequence = factor(phys_health_consequence, levels = c("No", "Maybe", "Yes")),
        coworkers = factor(coworkers, levels = c("No", "Some of them", "Yes")),
        supervisor = factor(supervisor, levels = c("No", "Some of them", "Yes")),
        mental_health_interview = factor(mental_health_interview, levels = c("No", "Maybe", "Yes")),
        phys_health_interview = factor(phys_health_interview, levels = c("Maybe", "No", "Yes")),
        mental_vs_physical = factor(mental_vs_physical, levels = c("No", "Don't know", "Yes")),
        obs_consequence = factor(obs_consequence, levels = c("No", "Yes"))
    )

    for(colName in names(newdata)) {
        print(colName)
        print(model$forest$xlevels[[colName]])
        levels(newdata[[colName]]) = model$forest$xlevels[[colName]]
    }

    pred <- predict(model, newdata = newdata, na.action = na.roughfix, type= "prob")
    
    return(toJSON(data.frame(pred)))
}
