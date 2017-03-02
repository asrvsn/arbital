import React, { Component, PropTypes } from 'react'

import {Toolbar, ToolbarGroup, ToolbarSeparator, ToolbarTitle} from 'material-ui/Toolbar';
import FontIcon from 'material-ui/FontIcon';
import RaisedButton from 'material-ui/RaisedButton';

const logout = () => {
  // TODO
}

const Header = (props) => {
  const { user } = props

  return (
    <Toolbar>
      <ToolbarGroup firstChild={true}>
        {user.name}
      </ToolbarGroup>
      <ToolbarGroup>
        <ToolbarTitle text="Options" />
        <FontIcon className="muidocs-icon-custom-sort" />
        <ToolbarSeparator />
        <RaisedButton label="Logout" secondary={true} onTouchTap={e => logout() } />
      </ToolbarGroup>
    </Toolbar>
  )
}

export default Header
