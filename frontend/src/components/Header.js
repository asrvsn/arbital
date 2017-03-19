import React, { Component, PropTypes } from 'react'

import {Toolbar, ToolbarGroup, ToolbarSeparator, ToolbarTitle} from 'material-ui/Toolbar';
import FontIcon from 'material-ui/FontIcon';
import RaisedButton from 'material-ui/RaisedButton';

import AuthorChip from './AuthorChip'
import AuthenticatedHOC from '../hoc/AuthenticatedHOC'

const logout = (props) => {
  props.router.push('/logout')
}

const Header = (props) => {
  const { session, isAuthenticated } = props

  if (isAuthenticated) {
    return (
      <Toolbar>
        <ToolbarGroup firstChild={true}>
          <ToolbarTitle text="Arbital" />
        </ToolbarGroup>
        <ToolbarGroup>
          <AuthorChip
            authorName={session.user.name}
            authorId={session.user.id}
          />
          <ToolbarSeparator />
          <RaisedButton
            label="Logout"
            secondary={true}
            onTouchTap={e => logout(props)}
          />
        </ToolbarGroup>
      </Toolbar>
    )
  } else {
    return <noscript />
  }
}

export default AuthenticatedHOC(Header)
