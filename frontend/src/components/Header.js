import React, { Component, PropTypes } from 'react'

import {Toolbar, ToolbarGroup, ToolbarSeparator, ToolbarTitle} from 'material-ui/Toolbar';
import FontIcon from 'material-ui/FontIcon';
import RaisedButton from 'material-ui/RaisedButton';
import ActionHome from 'material-ui/svg-icons/action/home';
import ActionSearch from 'material-ui/svg-icons/action/search';
import IconButton from 'material-ui/IconButton';
import Popover from 'material-ui/Popover';
import IconMenu from 'material-ui/IconMenu';
import MenuItem from 'material-ui/MenuItem';

import Finder from './Finder'
import AuthorChip from './AuthorChip'
import AuthenticatedHOC from './hoc/AuthenticatedHOC'

const styles = {
  title: {
    marginLeft: 16
  },
  searchPopover: {
    width: 400
  },
  userMenu: {
    marginLeft: 20
  }
}

class Header extends Component {
  constructor(props) {
    super(props)
    this.state = {
      searchOpen: false,
      searchIconElem: null
    }
  }

  render() {
    const { session, isAuthenticated, router } = this.props
    const { finderOpen, searchIconElem } = this.state

    const goHome = () => router.push('/')
    const goToUser = () => router.push(`/users/${session.user.id}`)
    const logout = () => router.push('/logout')

    if (isAuthenticated) {
      return (
        <Toolbar>

          <ToolbarGroup firstChild={true}>
            <ToolbarTitle text="Arbital" style={styles.title} />
          </ToolbarGroup>

          <ToolbarGroup>

            <IconButton
              tooltip="search"
              onTouchTap={e => this.openFinder(e)}
            >
              <ActionSearch />
            </IconButton>

            <Popover
              open={finderOpen}
              anchorEl={searchIconElem}
              anchorOrigin={{horizontal: 'right', vertical: 'bottom'}}
              targetOrigin={{horizontal: 'right', vertical: 'top'}}
              onRequestClose={() => this.closeFinder(null)}
              style={styles.searchPopover}
            >
              <Finder
                onRequestClose={(result) => this.closeFinder(result)}
                dropDownEnabled={true}
              />
            </Popover>

            <IconButton tooltip="Home" onTouchTap={e => goHome()}>
              <ActionHome />
            </IconButton>

            <ToolbarSeparator />

            <IconMenu
              iconButtonElement={
                <AuthorChip
                  authorName={session.user.name}
                  authorId={session.user.id}
                />
              }
              anchorOrigin={{horizontal: 'right', vertical: 'bottom'}}
              targetOrigin={{horizontal: 'right', vertical: 'top'}}
              style={styles.userMenu}
            >
              <MenuItem
                primaryText="Profile"
                onTouchTap={e => goToUser()}
              />
              <MenuItem
                primaryText="Logout"
                onTouchTap={e => logout()}
              />
            </IconMenu>

          </ToolbarGroup>
        </Toolbar>
      )
    } else {
      return <noscript />
    }
  }

  openFinder(e) {
    this.setState({
      finderOpen: true,
      searchIconElem: e.currentTarget
    })
  }

  closeFinder(result) {
    this.setState({ finderOpen: false })

    if (!! result) {
      const { router } = this.props
      switch(result.type) {
        case 'claims': {
          router.push(`/claims/${result.id}`)
          return
        }
        case 'arguments': {
          router.push(`/arguments/${result.id}`)
          return
        }
        case 'users': {
          router.push(`/users/${result.id}`)
          return
        }
        default: {
          throw 'finder got weird result'
        }
      }
    }
  }
}

export default AuthenticatedHOC(Header)
