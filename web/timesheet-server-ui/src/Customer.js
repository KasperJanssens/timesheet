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
    NumberInput, required
} from 'react-admin';

export const CustomerList = props => {
    return (
        <List {...props}>
            <Datagrid rowClick="show">
                <TextField source={"name"} label="Company Name"/>
                <TextField source={"vatNumber"} label={"VAT Number"}/>
                <TextField source={"addressStreet"} label={"Street"}/>
                <TextField source={"addressCity"} label={"City"}/>
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
                <TextField source={"addressStreet"} label={"Street"}/>
                <TextField source={"addressCity"} label={"City"}/>
                <NumberField source={"hourlyRate"} label={"Hourly Rate"}/>
                <NumberField source={"paymentTerm"} label={"Payment Term (in days)"}/>
            </SimpleShowLayout>
        </Show>
    )
}

export const CustomerCreate = props => {
    return (
        <Create  {...props}>
            <SimpleForm>
                <TextInput source={"name"} label={"Company Name"} validate={required()}/>
                <TextInput source={"vatNumber"} label={"VAT Number"} validate={required()}/>
                <TextInput source={"addressStreet"} label={"Street"} validate={required()}/>
                <TextInput source={"addressCity"} label={"City"} validate={required()}/>
                <NumberInput source={"hourlyRate"} label={"Hourly Rate"} validate={required()}/>
                <NumberInput source={"paymentTerm"} label={"Payment Term (in days)"}
                             validate={required()}/>
            </SimpleForm>
        </Create>
    )
}