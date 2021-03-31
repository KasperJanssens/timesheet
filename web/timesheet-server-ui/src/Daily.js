import React from 'react';
import {
    Datagrid,
    DateField,
    DateInput,
    List,
    ArrayField,
    TextField,
    TextInput,
    SelectField,
    SelectInput,
    SimpleForm,
    UrlField,
    ShowButton,
    Loading,
    Error,
    Show,
    SimpleShowLayout,
    Create,
    Edit,
    SimpleFormIterator,
    ArrayInput,
    NumberField,
    NumberInput
} from 'react-admin';
import { Link, BrowserRouter as Router, Route, Switch } from "react-router-dom";

import {useGetList} from "ra-core";
import {Krondorsoft_invoice} from "./krondorsoft_invoice";

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
                <DateField source={"day"} locales="nl-BE"/>

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
    if (loadingWorkTypes) return <Loading/>;
    if (errorWorkTypes) return <Error error={"Could not load work types"}/>;

    return (<Create {...props}>
        <SimpleForm>
            <DateInput source={"day"}/>
            <ArrayInput source="workpacks" label="WorkPacks">
                <SimpleFormIterator>
                    <SelectInput source={"workType"} choices={Object.values(tps)} label={"type of work"}/>
                    <TextInput source={"description"} label={"description"}/>
                    <NumberInput source="amount" step={0.5} label={"hours"}/>
                </SimpleFormIterator>
            </ArrayInput>
        </SimpleForm>
    </Create>)
}

export const DailyEdit = props => {
    const {
        data: worktypes,
        loading: loadingRules,
        error: errorRules
    } = useGetList('worktype', {}, {}, {});
    if (loadingRules) return <Loading/>;
    if (errorRules) return <Error error={"Could not load work types"}/>;
    return (<Edit {...props}>
        <SimpleForm>
            <DateInput source={"day"}/>
            <ArrayInput source="workpacks" label="WorkPacks">
                <SimpleFormIterator>
                    <SelectInput source={"workType"} choices={Object.values(worktypes)} label={"type of work"}/>
                    <TextInput source={"description"} label={"description"}/>
                    <NumberInput source="amount" step={0.5} label={"hours"}/>
                </SimpleFormIterator>
            </ArrayInput>
        </SimpleForm>
    </Edit>)
}