import { Layout } from 'react-admin';
import MyMenu from './Menu';
import React from "react";

const MyLayout = (props) => <Layout {...props} menu={MyMenu} />;

export default MyLayout;