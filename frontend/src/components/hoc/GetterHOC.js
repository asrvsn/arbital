import React, { Component } from 'react'
import Redbox from 'redbox-react'
import { connect } from 'react-redux'
import shortid from 'shortid'

import LinearProgress from 'material-ui/LinearProgress';

import backend from '../../util/backend'
import { registerReloadListener, unregisterReloadListener } from '../../actions'

import AuthenticatedHOC from './AuthenticatedHOC'

/**
  NOTE: if using this HOC, do not use AuthenticatedHOC!
  **/

const styles = {
  loader: {
    top: '50%',
    left: '50%',
    transform: 'translateY(-50%, -50%)'
  }
}

export default (ChildComponent, getter) => {

  const name = shortid.generate()

  class GetterComponent extends Component {
    constructor(props) {
      super(props)
      this.state = {
        childProps: {},
        gettingState: 'LOADING',
        error: ''
      }
      this.backendUrl = window.location.hostname + '5000'
    }

    componentDidMount() {
      this.loadAllData()
      // Register reload listeners for each datasource
      const { dispatch } = this.props
      dispatch(registerReloadListener(
        name,
        (dataSource) => this.loadData(dataSource)
      ))
    }

    componentWillUnmount() {
      // Unregister reload listeners
      const { dispatch } = this.props
      dispatch(unregisterReloadListener(name))
    }

    render() {
      const { childProps, gettingState, error } = this.state

      if (Object.keys(getter(this.props)).length == 0) {

        return <ChildComponent {...this.props} />

      } else {
        switch(gettingState) {
          case 'LOADING':
            return (
              <div style={styles.loader}>
                <span>Fetching data</span>
                <LinearProgress mode="indeterminate" />
              </div>
            )
          case 'FAILED':
            return <h1>{"Error fetching data: " + error}</h1>
          case 'SUCCEEDED': {
            const allChildProps = Object.assign({},
              this.props,
              childProps
            )
            return <ChildComponent {...allChildProps} />
          }

        }
      }

    }

    loadData(dataSource) {
      const getterP = getter(this.props)
      if (dataSource in getterP) {
        console.warn(name + ' loading datasource: ' + dataSource)
        const { session } = this.props
        const { path, mapResponseToProps } = getterP[dataSource]
        backend
          .authenticate(session.id)
          .get(path, (err, response, body) => {
            if (err !== null) {
              this.setState({gettingState: 'FAILED', error: err})
            } else {
              if (response.statusCode == 200) {
                const { childProps } = this.state
                const otherProps = mapResponseToProps(JSON.parse(body))
                if (typeof otherProps == "object") {
                  const newChildProps = Object.assign(
                    {},
                    childProps,
                    otherProps
                  )
                  this.setState({childProps: newChildProps, gettingState: 'SUCCEEDED'})
                } else {
                  const err = 'mapResponseToProps expected object, got ' + String(otherProps)
                  this.setState({gettingState: 'FAILED', error: err})
                  throw err
                }
              } else {
                this.setState({gettingState: 'FAILED', error: response.statusMessage})
              }
            }
          })
      }
    }

    loadAllData() {
      const getterP = getter(this.props)
      Object.keys(getterP).forEach(
        (dataSource) => this.loadData(dataSource)
      )
    }
  }

  return AuthenticatedHOC(
    connect(
      null,
      (dispatch) => ({ dispatch })
    )(GetterComponent)
  )
}
