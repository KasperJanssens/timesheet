import React from 'react';
import {
    ArrayField,
    ArrayInput,
    Create,
    Datagrid,
    DateField,
    DateInput,
    Edit,
    Error,
    List,
    Loading,
    NumberField,
    NumberInput,
    SelectField,
    SelectInput,
    SimpleForm,
    SimpleFormIterator,
    TextField,
    TextInput,
    useLocale
} from 'react-admin';

import {useGetList} from "ra-core";
import {toSelectCompanies, toSelectCustomers} from "./helper";

const toChoices = items => items.map(item => ({id: item, name: item}));

export const DailyList = props => {
    const {
        data: worktypes,
        loading: loadingRules,
        error: errorRules
    } = useGetList('worktype', {}, {}, {});
    if (loadingRules) return <Loading/>;
    if (errorRules) return <Error error={"Could not load work types"}/>;
    return (
        <List {...props}>
            <Datagrid rowClick="show">
                <DateField source={"day"} locales={"nl-BE"}/>

                <ArrayField source="workpacks" label="WorkPacks">
                    <Datagrid>
                        <SelectField source={"workType"} choices={Object.values(worktypes)}/>
                        <TextField source={"description"}/>
                        <NumberField source="amount" options={{maximumFractionDigits: 1}}/>
                    </Datagrid>
                </ArrayField>

            </Datagrid>
        </List>
    );
}


export const DailyCreate = props => {
    const {
        data: tps,
        loading: loadingWorkTypes,
        error: errorWorkTypes
    } = useGetList('worktype', {
        page: 1,
        perPage: 1000
    }, {}, {});


    const {
        data: companies,
        loading: loadingCompanies,
        error: errorCompanies
    } = useGetList('company', {
        page: 1,
        perPage: 1000
    }, {}, {});


    const {
        data: customers,
        loading: loadingCustomers,
        error: errorCustomers
    } = useGetList('customer', {
        page: 1,
        perPage: 1000
    }, {}, {});

    if (loadingWorkTypes) return <Loading/>;
    if (errorWorkTypes) return <Error error={"Could not load work types"}/>;
    if (loadingCompanies) return <Loading/>;
    if (errorCompanies) return <Error error={"Could not load companies"}/>;
    if (loadingCustomers) return <Loading/>;
    if (errorCustomers) return <Error error={"Could not load customers"}/>;

    let selectCustomers = toSelectCustomers(Object.values(customers))
    let selectCompanies = toSelectCompanies(Object.values(companies))

    return (<Create {...props}>
        <SimpleForm>
            <DateInput source={"day"}  locales={"nl-BE"}/>
            <ArrayInput source="workpacks" label="WorkPacks">
                <SimpleFormIterator>
                    <SelectInput source={"workType"} choices={Object.values(tps)}
                                 label={"type of work"}/>
                    <TextInput source={"description"} label={"description"}/>
                    <NumberInput source="amount" step={0.5} label={"hours"}/>

                </SimpleFormIterator>
            </ArrayInput>
            <SelectInput source={"customerId"} choices={selectCustomers}
                         label={"choose customer"}/>
            <SelectInput source={"companyVat"} choices={selectCompanies}
                         label={"choose company"}/>
        </SimpleForm>
    </Create>)
}


export const DailyEdit = props => {
    const {
        data: worktypes,
        loading: loadingRules,
        error: errorRules
    } = useGetList('worktype', {}, {}, {});

    const {
        data: companies,
        loading: loadingCompanies,
        error: errorCompanies
    } = useGetList('company', {
        page: 1,
        perPage: 1000
    }, {}, {});

    const {
        data: customers,
        loading: loadingCustomers,
        error: errorCustomers
    } = useGetList('customer', {
        page: 1,
        perPage: 1000
    }, {}, {});

    if (loadingRules) return <Loading/>;
    if (errorRules) return <Error error={"Could not load work types"}/>;
    if (loadingCompanies) return <Loading/>;
    if (errorCompanies) return <Error error={"Could not load companies"}/>;
    if (loadingCustomers) return <Loading/>;
    if (errorCustomers) return <Error error={"Could not load customers"}/>;

    let selectCustomers = toSelectCustomers(Object.values(customers))
    let selectCompanies = toSelectCompanies(Object.values(companies))


    return (<Edit {...props}>
        <SimpleForm>
            <DateInput source={"day"}  locales={"nl-BE"}/>
            <ArrayInput source="workpacks" label="WorkPacks">
                <SimpleFormIterator>
                    <SelectInput source={"workType"} choices={Object.values(worktypes)}
                                 label={"type of work"}/>
                    <TextInput source={"description"} label={"description"}/>
                    <NumberInput source="amount" step={0.5} label={"hours"}/>

                </SimpleFormIterator>
            </ArrayInput>
            <SelectInput source={"customerId"} choices={selectCustomers}
                         label={"choose customer"}/>
            <SelectInput source={"companyVat"} choices={selectCompanies}
                         label={"choose company"}/>
        </SimpleForm>
    </Edit>)
}