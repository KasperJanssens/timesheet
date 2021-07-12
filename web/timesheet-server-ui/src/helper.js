export const toSelectCustomers = customers => customers.map(customer => ({
    id: customer.id,
    name: customer.name
}))
export const toSelectCompanies = companies => companies.map(company => ({
    id: company.id,
    name: company.name
}))

export const toSelectQuotes = quotes => quotes.map(quote => ({
    id: quote.id,
    name: "#"+quote.quoteId + " from " + quote.company.name + " to " + quote.customer.name + " for a value excl vat of " +quote.vatReport.totalExcl
}))