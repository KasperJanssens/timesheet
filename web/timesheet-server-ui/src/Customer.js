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

export const CustomerList = props => {
    return (
        <List {...props}>
            <Datagrid rowClick="show">
                <TextField source={"name"} label="Company Name"/>
                <TextField source={"vatNumber"} label={"VAT Number"}/>
            </Datagrid>
        </List>
    )
}

export const CustomerShow = props => {
    return (
        <Show {...props}>
            <SimpleShowLayout>
                <TextField source={"name"} label={"Company Name"}/>
                <TextField source={"vatNumber"} label={"VAT Number"}/>
                <NumberField source={"hourlyRate"} label={"Hourly Rate"}/>
            </SimpleShowLayout>
        </Show>
    )
}

export const CustomerCreate = props => {
    return (
        <Create  {...props}>
            <SimpleForm>
                <TextInput source={"name"} label={"Company Name"}/>
                <TextInput source={"vatNumber"} label={"VAT Number"}/>
                <NumberInput source={"hourlyRate"} label={"Hourly Rate"}/>
            </SimpleForm>
        </Create>
    )
}