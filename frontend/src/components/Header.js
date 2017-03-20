import React, { Component, PropTypes } from 'react'

import {Toolbar, ToolbarGroup, ToolbarSeparator, ToolbarTitle} from 'material-ui/Toolbar';
import FontIcon from 'material-ui/FontIcon';
import RaisedButton from 'material-ui/RaisedButton';
import ActionHome from 'material-ui/svg-icons/action/home';
import IconButton from 'material-ui/IconButton';

import AuthorChip from './AuthorChip'
import AuthenticatedHOC from '../hoc/AuthenticatedHOC'

const styles = {
  title: {
    marginLeft: 16
  }
}

const logout = (props) => {
  props.router.push('/logout')
}

const Header = (props) => {
  const { session, isAuthenticated, router } = props
  const goHome = () => router.push('/')

  if (isAuthenticated) {
    return (
      <Toolbar>
        <ToolbarGroup firstChild={true}>
          <ToolbarTitle text="Arbital" style={styles.title} />
          <IconButton tooltip="Home" onTouchTap={e => goHome()}>
            <ActionHome />
          </IconButton>
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
