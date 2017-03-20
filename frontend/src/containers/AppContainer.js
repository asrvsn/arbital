import React, { Component, PropTypes } from 'react'
import { browserHistory, Router, Route, IndexRoute } from 'react-router'
import { Provider } from 'react-redux'

import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';

import PageHOC from '../components/hoc/PageHOC'

import Feed from '../components/pages/Feed'
import Argument from '../components/pages/Argument'
import Claim from '../components/pages/Claim'
import User from '../components/pages/User'
import Login from '../components/pages/Login'
import Logout from '../components/pages/Logout'

class AppContainer extends Component {
  static propTypes = {
    store  : PropTypes.object.isRequired
  }

  shouldComponentUpdate () {
    return false
  }

  render () {
    const { store } = this.props

    const requireAuth = (nextState, replace) => {
      if (! store.getState().isAuthenticated) {
        replace('/login')
      }
    }

    const children = (
      <div>
        <Route path='/'
               component={PageHOC(Feed)}
               onEnter={requireAuth}
               />

        <Route path='/login'
               component={Login}
               />

        <Route path='/logout'
               component={Logout}
               onEnter={requireAuth}
               />

        <Route path='/arguments/:argumentid'
               component={PageHOC(Argument)}
               onEnter={requireAuth}
               />

        <Route path='/claims/:claimid'
               component={PageHOC(Claim)}
               onEnter={requireAuth}
               />

        <Route path='/users/:userid'
               component={PageHOC(User)}
               onEnter={requireAuth}
               />
      </div>
    )

    return (
      <Provider store={store}>
        <MuiThemeProvider>
          <div style={{ height: '100%' }}>
            <Router history={browserHistory} children={children} />
          </div>
        </MuiThemeProvider>
      </Provider>
    )
  }
}


export default AppContainer
