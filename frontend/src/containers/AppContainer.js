import React, { Component, PropTypes } from 'react'
import { browserHistory, Router, Route, IndexRoute } from 'react-router'
import { Provider } from 'react-redux'

import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';

import Header from '../components/Header'
import Feed from '../components/Feed'
import Argument from '../components/Argument'
import Claim from '../components/Claim'
import CreateArgument from '../components/CreateArgument'
import CreateClaim from '../components/CreateClaim'
import Login from '../components/Login'
import Logout from '../components/Logout'
import User from '../components/User'

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
        <Header />
        <Route path='/' component={Feed} onEnter={requireAuth} />
        <Route path='/feed' component={Feed} onEnter={requireAuth} />

        <Route path='/login' component={Login} />
        <Route path='/logout' component={Logout} onEnter={requireAuth} />

        <Route path='/arguments/:argumentid' component={Argument} onEnter={requireAuth} />

        <Route path='/claims/:claimid' component={Claim} onEnter={requireAuth} />
        <Route path='/claims/:claimid/for' component={CreateArgument} onEnter={requireAuth} />
        <Route path='/claims/:claimid/against' component={CreateArgument} onEnter={requireAuth} />
        <Route path='/claims/:claimid/create' component={CreateClaim} onEnter={requireAuth} />

        <Route path='/users/:userid' component={User} onEnter={requireAuth} />
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
